#![allow(non_snake_case)]
#![allow(non_camel_case_types)]

use libc::{c_int, c_char, c_void, c_double};

use crate::iocsh;

#[no_mangle]
pub static mut iocshPpdbbase: usize = 0;

#[repr(C)]
pub enum iocshArgType {
    iocshArgInt,
    iocshArgDouble,
    iocshArgString,
    iocshArgPdbbase,
    iocshArgArgv,
    iocshArgPersistentString
}

#[repr(C)]
pub struct iocshArgBufArgv {
    ac: c_int,
    av: *mut *mut c_char,
}

#[repr(C)]
pub union iocshArgBuf {
    pub ival: c_int,
    pub dval: c_double,
    pub sval: *const c_char,
    pub vval: *const c_void,
    //aval: iocshArgBufArgv, // not supported
    pub aval: [u8; 16], // c_int(4) + ptr(8) + alignment
}

#[repr(C)]
pub struct iocshVarDef {
    pub name: *const c_char,
    pub type_: iocshArgType,
    pub pval: *mut c_void,
}

#[repr(C)]
pub struct iocshArg {
    pub name: *const c_char,
    pub type_: iocshArgType,
}

#[repr(C)]
pub struct iocshFuncDef {
    pub name: *const c_char,
    pub nargs: c_int,
    pub arg: *const *const iocshArg,
}

pub type iocshCallFunc = extern fn(argBuf: *const iocshArgBuf);

#[repr(C)]
pub struct iocshCmdDef {
    pub pFuncDef: *const iocshFuncDef,
    pub func: *const iocshCallFunc,
}

#[no_mangle]
pub unsafe extern "C"
fn iocshRegister(piocshFuncDef: *const iocshFuncDef,
    func: iocshCallFunc) {

    iocsh::register( (&( &*piocshFuncDef, func)).into() );
}

#[no_mangle]
pub unsafe extern "C"
fn iocshRegisterVariable(piocshVarDef: *const iocshVarDef) {
    iocsh::register_variable((&*piocshVarDef).into());
}

#[no_mangle]
pub unsafe extern "C"
fn iocshFindCommand(_name: *const c_char) -> *const iocshCmdDef {
    std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C"
fn iocshFindVariable(_name: *const c_char) -> *const iocshVarDef {
    std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C"
fn iocshFree() {
    eprintln!("iocshFree()");
}

#[no_mangle]
pub unsafe extern "C"
fn iocsh(pathname: *const c_char) -> c_int {
    iocshLoad(pathname, std::ptr::null())
}

#[no_mangle]
pub unsafe extern "C"
fn iocshCmd(cmd: *const c_char) -> c_int {
    iocshRun(cmd, std::ptr::null())
}

#[no_mangle]
pub unsafe extern "C"
fn iocshLoad(pathname: *const c_char, macros: *const c_char) -> c_int {
    if !pathname.is_null() {

    }
    iocshBody(pathname, std::ptr::null(), macros)
}

#[no_mangle]
pub unsafe extern "C"
fn iocshRun(cmd: *const c_char, macros: *const c_char) -> c_int {
    if cmd.is_null() {
        0
    } else {
       iocshBody(std::ptr::null(), cmd, macros)
    }
}

fn iocshBody(path: *const c_char, cmd: *const c_char, macros: *const c_char) -> c_int {
    let path = iocsh::try_from_c_str(path);
    let cmd = iocsh::try_from_c_str(cmd);
    let macros = iocsh::try_from_c_str(macros);

    let mode = if cmd.is_some() {
        iocsh::Mode::CmdLine(cmd.unwrap())
    } else if path.is_none() {
        iocsh::Mode::Interactive
    } else {
        iocsh::Mode::PathName(path.unwrap())
    };

    match iocsh::run(mode, macros) {
        Ok(_) => 0,
        Err(e) => e as c_int
    }
}

#[no_mangle]
pub unsafe extern "C"
fn iocshEnvClear(name: *const c_char) {
    match iocsh::try_from_c_str(name) {
        Some(s) => iocsh::env_clear(s.as_str()),
        None => {}
    };
}
