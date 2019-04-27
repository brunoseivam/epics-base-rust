extern crate lazy_static;

use std::sync::RwLock;
use std::sync::Arc;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::env;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use regex::Regex;
use std::cell::RefCell;

use std::ffi::{CStr, CString};
use libc::{c_char, c_int, c_double, c_void};

use crate::mac;
use crate::ffi;

// Implemented elsewhere
extern "C" {
    fn epicsEnvSet(name: *const c_char, value: *const c_char);
}

// Utility functions
pub fn try_from_c_str(s:*const c_char) -> Option<String> {
    if s.is_null() {
        None
    } else {
        unsafe {
            let c_str = CStr::from_ptr(s);
            let r_str = c_str.to_str().expect("from_c_str got invalid string");
            Some(r_str.into())
        }
    }
}

pub fn from_c_str(s: *const c_char) -> String {
    match try_from_c_str(s) {
        Some(x) => x,
        None => String::new()
    }
}

#[derive(Debug)]
pub enum ArgType {
    Int,
    Double,
    String,
    Pdbbase,
    Array,
}

impl From<&ffi::iocshArgType> for ArgType {
    fn from(t: &ffi::iocshArgType) -> Self {
        use ffi::iocshArgType::*;

        match t {
            iocshArgInt => ArgType::Int,
            iocshArgDouble => ArgType::Double,
            iocshArgPdbbase => ArgType::Pdbbase,
            iocshArgString | iocshArgPersistentString => ArgType::String,
            iocshArgArgv => ArgType::Array,
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum ArgBuf {
    Int(i32),
    Double(f64),
    String(Option<CString>),
    Pdbbase,
    Array(Vec<CString>),
}

impl From<&ffi::iocshArgType> for ArgBuf {
    fn from(t: &ffi::iocshArgType) -> Self {
        use ffi::iocshArgType::*;

        match t {
            iocshArgInt => ArgBuf::Int(0),
            iocshArgDouble => ArgBuf::Double(0.0),
            iocshArgPdbbase => ArgBuf::Pdbbase,
            iocshArgString | iocshArgPersistentString =>
                ArgBuf::String(None),
            iocshArgArgv => ArgBuf::Array(Vec::new()),
        }
    }
}

// Convert from Rust args to C args: &[ArgBuf] to &[iocshArgBuf]
fn from_rargs(rargs: &[ArgBuf]) -> Vec<ffi::iocshArgBuf> {
    let mut result = Vec::with_capacity(rargs.len());

    for ra in rargs.iter() {
        result.push(
            match ra {
                ArgBuf::Int(i) => ffi::iocshArgBuf { ival: *i },
                ArgBuf::Double(d) => ffi::iocshArgBuf { dval: *d },
                ArgBuf::String(s) => ffi::iocshArgBuf {
                    sval: match s {
                        Some(s) => s.as_ptr(),
                        None => std::ptr::null()
                    }
                },
                ArgBuf::Pdbbase => ffi::iocshArgBuf {
                    vval: unsafe { ffi::iocshPpdbbase as *const c_void }
                },
                ArgBuf::Array(_) => unimplemented!()
            }
        )
    }

    result
}

#[derive(Debug)]
enum VarVal {
    Int(usize),
    Double(usize),
}

#[derive(Debug)]
pub struct VarDef {
    name: String,
    value: VarVal
}

impl From<&ffi::iocshVarDef> for VarDef {
    fn from(v: &ffi::iocshVarDef) -> Self {
        use ffi::iocshArgType::*;

        VarDef {
            name: from_c_str(v.name),
            value: match v.type_ {
                iocshArgInt => VarVal::Int(v.pval as usize),
                iocshArgDouble => VarVal::Double(v.pval as usize),
                _ => unimplemented!()
            }
        }
    }
}

#[derive(Debug)]
pub struct Arg {
    name: String,
    type_: ArgType
}

impl Arg {
    fn new(name: &str, type_: ArgType) -> Arg {
        Arg { name: name.to_owned(), type_: type_ }
    }
}

impl From<&ffi::iocshArg> for Arg {
    fn from(a: &ffi::iocshArg) -> Self {
        Arg {
            name: from_c_str(a.name),
            type_: ArgType::from(&a.type_),
        }
    }
}

#[derive(Debug)]
pub struct FuncDef {
    name: String,
    args: Vec<Arg>
}

impl FuncDef {
    fn new(name: &str, args: Vec<Arg>) -> FuncDef {
        FuncDef {name: name.to_owned(), args: args }
    }
}

impl From<&ffi::iocshFuncDef> for FuncDef {
    fn from(f: &ffi::iocshFuncDef) -> Self {
        let mut args = Vec::new();

        for i in 0..(f.nargs) {
            unsafe {
                let arg = *f.arg.offset(i as isize);
                args.push(Arg::from(&*arg));
            }
        }

        FuncDef {
            name: from_c_str(f.name),
            args: args
        }
    }
}

pub enum CallFunc {
    CFunc(ffi::iocshCallFunc),
    RFunc(fn(&[ArgBuf])),
}

impl From<&ffi::iocshCallFunc> for CallFunc {
    fn from(f: &ffi::iocshCallFunc) -> Self {
        CallFunc::CFunc(*f)
    }
}

pub struct CmdDef {
    func_def: FuncDef,
    func: CallFunc
}

impl CmdDef {
    fn new(name: &str, args: Vec<Arg>, func: CallFunc) -> CmdDef {
        CmdDef {
            func_def: FuncDef::new(name, args),
            func: func
        }
    }
}

impl From<&(&ffi::iocshFuncDef, ffi::iocshCallFunc)> for CmdDef {
    fn from(c: &(&ffi::iocshFuncDef, ffi::iocshCallFunc)) -> Self {
        CmdDef {
            func_def: FuncDef::from(c.0),
            func: CallFunc::from(&c.1),
        }
    }
}

// iocsh functions provided by this module

fn help_func(args: &[ArgBuf]) {
    match args {
        [ArgBuf::String(None)] => {
            println!("Type 'help <command>' to see the arguments of <command>.");

            let cmds = find_commands();

            let mut col = 0usize;
            for cmd in cmds.iter() {
                let l = cmd.len();

                if l + col >= 79 {
                    println!("");
                    col = 0;
                }

                print!("{}", cmd);
                col += l;

                if col >= 64 {
                    println!("");
                    col = 0;
                } else {
                    col -= 1;
                    while col % 16 != 0 {
                        print!(" ");
                        col += 1;
                    }
                }
            }
            if col > 0 {
                println!("");
            }
        },

        [ArgBuf::String(Some(ref cmd))] => {
            let cmd = cmd.to_str().unwrap();

           let cmd_def = match find_command(cmd) {
                Some(cmd_def) => cmd_def,
                None => return
            };

            print!("{}", cmd);

            for arg in cmd_def.func_def.args.iter() {
                if arg.name.contains(" ") {
                    print!(" '{}'", arg.name);
                } else {
                    print!(" {}", arg.name);
                }
            }
            println!("");
        },
        _ => unimplemented!()
    }
}

fn var_func(args: &[ArgBuf]) {
    fn handle_var(def: &VarDef, name: &str, val: Option<&str>) {
        match val {
            None => {
                print!("{} = ", name);
                match def.value {
                    VarVal::Int(p) => println!("{}", unsafe { *(p as *const c_int) }),
                    VarVal::Double(p) => println!("{}", unsafe { *(p as *const c_double) }),
                }
            },
            Some(val) => {
                match def.value {
                    VarVal::Int(p) => match val.parse::<c_int>() {
                        Ok(val) => unsafe { *(p as *mut c_int) = val },
                        Err(_) => eprintln!("Invalid integer value. Var {} not changed.", name)
                    },
                    VarVal::Double(p) => match val.parse::<c_double>() {
                        Ok(val) => unsafe { *(p as *mut c_double) = val },
                        Err(_) => eprintln!("Invalid double value. Var {} not changed.", name)
                    }
                }
            }
        }
    }

    match args {
        [ArgBuf::String(None), _] => {
            for_all_variables(|name, def| handle_var(def, name, None));
        },
        [ArgBuf::String(Some(ref name)), ArgBuf::String(ref val)] => {
            let name = name.to_str().unwrap();
            let val = val.as_ref().map(|s| s.to_str().unwrap());

            match find_variable(name) {
                Some(def) => handle_var(&def, name, val),
                None => eprintln!("Var {} not found.", name)
            }
        },

        _ => unimplemented!()
    }
}

thread_local! {
    static HANDLE: RefCell<mac::Mac> = RefCell::new(
        mac::Mac::new(&vec![("", "environ")]).unwrap()
    )
}

lazy_static! {
    static ref CMD_REGISTRY: RwLock<HashMap<String, Arc<CmdDef>>> =
        RwLock::new({
            let mut m = HashMap::new();
            m.insert("var".to_owned(), Arc::new(
                CmdDef::new("var", vec![
                    Arg::new("[variable", ArgType::String),
                    Arg::new("[value]]", ArgType::String),
                ], CallFunc::RFunc(var_func))
            ));
            m.insert("help".to_owned(), Arc::new(
                CmdDef::new("help", vec![
                    Arg::new("[command ...]", ArgType::String)
                ], CallFunc::RFunc(help_func))
            ));
            m
        });

    static ref VAR_REGISTRY:  RwLock<HashMap<String, Arc<VarDef>>> =
        RwLock::new(HashMap::new());
}

/// Registers a command
pub fn register (def: CmdDef) {
    //eprintln!("register_command({:?})", def.func_def);
    CMD_REGISTRY.write().expect("HashMap lock was poisoned")
        .insert(def.func_def.name.clone(), Arc::new(def));
}

/// Retrieves a previously registered function with the given name
pub fn find_command(name: &str) -> Option<Arc<CmdDef>> {
    CMD_REGISTRY.read().expect("HashMap lock was poisoned")
        .get(name).map(|v| Arc::clone(v))
}

/// Gets all command names
fn find_commands() -> Vec<String> {
    let mut cmds: Vec<String> =
        CMD_REGISTRY.read().expect("HashMap lock was poisoned")
        .keys().map(|k| k.clone()).collect();
    cmds.sort_unstable();
    cmds
}

/// Register a variable
pub fn register_variable(def: VarDef) {
    //eprintln!("register_variable({})", def.name);
    VAR_REGISTRY.write().expect("HashMap lock was poisoned")
        .insert(def.name.clone(), Arc::new(def));
}

/// Retrieves a previously registered variable with the given name.
pub fn find_variable(name :&str) -> Option<Arc<VarDef>> {
    VAR_REGISTRY.read().expect("HashMap lock was poisoned")
        .get(name).map(|v| Arc::clone(v))
}

fn for_all_variables(f: fn(&str, &VarDef)) {
    VAR_REGISTRY.read().expect("HashMap lock was poisoned")
        .iter().for_each(|(name, def)| f(name, def));
}

struct Location<'a> {
    filename: &'a str,
    lineno: usize
}

/// Report an error
fn show_error(location: &Option<Location>, msg:&str) {
    // TODO: write to the file returned from epicsGetStdErr
    if let Some(l) = location {
        eprint!("{} line {}: ", l.filename, l.lineno);
    }
    eprintln!("{}", msg);
}

fn parse_arg(location: &Option<Location>, type_: &ArgType, arg: Option<&str>)
    -> Result<ArgBuf, ()> {

    match type_ {
        ArgType::Int => match arg {
            None => Ok(ArgBuf::Int(0)),
            Some(arg) => match arg.parse::<i32>() {
                Ok(v) => Ok(ArgBuf::Int(v)),
                Err(_) => {
                    show_error(&location, &format!("Illegal integer '{}'", arg));
                    Err(())
                }
            }
        },

        ArgType::Double => match arg {
            None => Ok(ArgBuf::Double(0.0)),
            Some(arg) => match arg.parse::<f64>() {
                Ok(v) => Ok(ArgBuf::Double(v)),
                Err(_) => {
                    show_error(&location, &format!("Illegal double '{}'", arg));
                    Err(())
                }
            }
        },

        ArgType::String => Ok(ArgBuf::String(arg.map(|a| CString::new(a).unwrap()))),

        ArgType::Pdbbase => match arg {
            None | Some("0") | Some("pdbbase") => Ok(ArgBuf::Pdbbase),
            _ => {
                show_error(&location,
                    &format!("Expecting 'pdbbase', got '{}'", arg.unwrap()));
                Err(())
            }
        },

        ArgType::Array => {
            show_error(&location, "Illegal argument type: Array");
            Err(())
        }
    }
}

fn parse_args(location: &Option<Location>, expected_types: &[&ArgType],
    raw_args: &[String]) -> Result<Vec<ArgBuf>,()> {

    let mut result = Vec::with_capacity(expected_types.len());

    for (i, et) in expected_types.iter().enumerate() {
        let raw_arg = raw_args.get(i).map(|v| (*v).as_str());
        let val = parse_arg(location, et, raw_arg)?;
        result.push(val);
    }

    return Ok(result);
}

enum Token {
    Word(String),
    InRedirect,
    OutRedirect(u8 /* fd */, bool /* append */)
}

enum Tokenized {
    Comment(String, bool),
    Tokens(Vec<Token>)
}

fn lex(line: &str) -> Result<Tokenized, &str> {
    lazy_static! {
        static ref COMMENT:Regex    = Regex::new(r#"^#(?P<suppress>-)?.*$"#).unwrap();
        static ref WHITESPACE:Regex = Regex::new(r#"^([ \t\(\),\r])+"#).unwrap();
        static ref DQUOTED:Regex    = Regex::new(r#"^"(?P<word>([^"]|\\")*)""#).unwrap();
        static ref SQUOTED:Regex    = Regex::new(r#"^'(?P<word>([^']|\\')*)'"#).unwrap();
        static ref BAREWORD:Regex   = Regex::new(r#"^(?P<word>([^# \t\(\),\r"'<>])+)"#).unwrap();
        static ref OUTREDIR:Regex   = Regex::new(r#"^(?P<fd>[1-9])?>(?P<append>>)?"#).unwrap();
        static ref INREDIR:Regex    = Regex::new(r#"^<"#).unwrap();
    }

    // Check for comments
    if let Some(c) = COMMENT.captures(line) {
        let suppress = c.name("suppress").is_some();
        return Ok(Tokenized::Comment(line.into(), suppress));
    }

    // Parse the line into tokens
    let mut i = 0usize;
    let mut tokens: Vec<Token> = Vec::new();

    'outer: while i < line.len() {
        let remainder = &line[i..];

        // Skip whitespace
        if let Some(m) = WHITESPACE.find(remainder) {
            i += m.end();
            continue 'outer;
        }

        // Look for output redirection
        if let Some(c) = OUTREDIR.captures(remainder) {
            let fd: u8 = c.name("fd").map_or(1, |v| v.as_str().parse().unwrap());
            let append = c.name("append").is_some();
            tokens.push(Token::OutRedirect(fd, append));
            i += c.get(0).unwrap().end();
            continue 'outer;
        }

        // Look for input redirection
        if let Some(m) = INREDIR.find(remainder) {
            tokens.push(Token::InRedirect);
            i += m.end();
            continue 'outer;
        }

        // Look for Words
        for re in [&*DQUOTED, &*SQUOTED, &*BAREWORD].iter() {
            if let Some(c) = re.captures(remainder) {
                let word = c.name("word").expect("expected word");
                tokens.push(Token::Word(word.as_str().into()));
                i += c.get(0).unwrap().end();
                continue 'outer;
            }
        }

        // If we got here, the line is invalid
        return Err("malformed line");
    }

    Ok(Tokenized::Tokens(tokens))
}

enum Command {
    Empty,
    InRedirect(String),
    /* function name, [args], (fd, append, filename)? */
    Call(String, Vec<String>, Option<(u8, bool, String)>),
}

fn parse(tokens: &[Token]) -> Result<Command, &str> {
    // Valid lines can be one of the following
    //   InRedirect Word(_)
    //   Word(_)*
    //   Word(_)+ OutRedirect(_) Word(_)

    if tokens.is_empty() {
        return Ok(Command::Empty);
    }

    if tokens.len() == 2 {
        match tokens {
            [Token::InRedirect, Token::Word(ref fname)] => {
                return Ok(Command::InRedirect(fname.clone()));
            },
            _ => {}
        }
    }

    if let Token::Word(ref cmd) = tokens[0] {
        let tokens = &tokens[1..];

        let args: Vec<String> = tokens.iter()
            .take_while(|t| match **t {
                Token::Word(_) => true,
                _ => false })
            .map(|t| match t {
                Token::Word(ref w) => w.clone(),
                _ => "".into()
            })
            .collect();

        let tokens = &tokens[args.len()..];

        let redir:Result<Option<(u8, bool, String)>,()> = match tokens {
            [Token::OutRedirect(fd, append), Token::Word(ref fname)] =>
                Ok(Some((*fd, *append, fname.clone()))),
            [] =>
                Ok(None),
            _ =>
                Err(())
        };

        return match redir {
            Ok(redir) => Ok(Command::Call(cmd.clone(), args, redir)),
            _ => Err("malformed command")
        };
    }

    return Err("malformed command");
}

#[derive(Debug)]
pub enum Mode {
    Interactive,
    CmdLine(String),
    PathName(String)
}

/// The body of the command interpreter
pub fn run (mode: Mode, macros: Option<String>) -> Result<(), i64> {

    let echo_comments = match mode {
        Mode::PathName(_) => true,
        _ => false
    };

    let prompt = match mode {
        Mode::Interactive => env::var("IOCSH_PS1").ok()
                .unwrap_or("epics> ".into()),
        _ => "".into()
    };

    let filename = match mode {
        Mode::PathName(ref f) => {
            let key = CString::new("IOCSH_STARTUP_SCRIPT").unwrap();
            let c_key = key.as_c_str();

            let val = CString::new(f.as_str()).unwrap();
            let c_val = val.as_c_str();
            unsafe { epicsEnvSet(c_key.as_ptr(), c_val.as_ptr()) };
            Some(f)
        },
        _ => None
    };

    let mut rl = Editor::<()>::new();
    let mut lineno:usize = 0;

    HANDLE.with(|h| {
        let h = h.borrow_mut();
        h.push_scope();
        if macros.is_some() {
            h.install_macros(
                &mac::parse_defns(macros.unwrap().as_str()).unwrap()
            );
        }
    });

    loop {
        let raw = rl.readline(&prompt);
        lineno += 1;

        let location = filename.as_ref().map_or(None, |f| Some(Location {
            filename: &f,
            lineno: lineno
        }));

        match raw {
            Ok(line) => {
                // Trim input string
                let line = line.trim();

                rl.add_history_entry(line);

                /*
                 * Ignore comment lines other than to echo
                 * them if they came from a script (disable echoing
                 * with '#-').  This avoids macLib errors from comments.
                 */
                let mut line_chars = line.chars();
                if line_chars.next() == Some('#') {
                    if line_chars.next() != Some('-') {
                        println!("{}", line);
                    }
                    continue;
                }

                // Expand macros
                let line = HANDLE.with(|h| h.borrow().def_expand(line).ok());

                if line.is_none() {
                    continue
                }

                // Trim whitespace coming from a macro
                let line = line.unwrap();
                let line = line.trim();

                // Lex the line
                let tokens = match lex(line) {
                    Err(err) => {
                        show_error(&location, err);
                        continue;
                    }

                    // Echo non-suppressed comment
                    Ok(Tokenized::Comment(text, suppress)) => {
                        if echo_comments && !suppress {
                            println!("{}", text);
                        }
                        continue;
                    },

                    // Extract tokens for further parsing
                    Ok(Tokenized::Tokens(v)) => v
                };

                // Syntatically parse the line
                match parse(&tokens) {
                    Err(msg) => {
                        show_error(&location, msg);
                        continue;
                    },
                    Ok(Command::Empty) => {
                        continue;
                    },
                    Ok(Command::InRedirect(_)) => {
                        continue;
                    },

                    Ok(Command::Call(ref cmd, _, _)) if cmd == "exit" => {
                        break;
                    },

                    Ok(Command::Call(cmd, args, _redir)) => {
                        let cmd_def = find_command(cmd.as_str());

                        if cmd_def.is_none() {
                            show_error(&location, &format!("Command {} not found", cmd));
                            continue;
                        }

                        let cmd_def = cmd_def.unwrap();

                        // Convert from Arg to ArgType, then pass them in
                        let expected_types: Vec<&ArgType> = cmd_def.func_def.args
                            .iter().map(|a| &a.type_).collect();

                        let cmd_args = parse_args(&location,
                            &expected_types, args.as_ref());

                        let cmd_args = match cmd_args {
                            Ok(cmd_args) => cmd_args,
                            Err(_) => continue,
                        };

                        match cmd_def.func {
                            CallFunc::RFunc(ref f) => f(&cmd_args),
                            CallFunc::CFunc(ref f) => {
                                let c_cmd_args = from_rargs(&cmd_args);
                                f(c_cmd_args.as_ptr());
                            }

                        }
                    }
                }
            },
            Err(ReadlineError::Interrupted) => {
                break;
            },
            Err(ReadlineError::Eof) => {
                break;
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    HANDLE.with(|h| h.borrow_mut().pop_scope());
    Ok(())
}

pub fn env_clear(name: &str) {
    HANDLE.with(|h| h.borrow_mut().clear_value(name));
}