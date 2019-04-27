/// Bindings for EPICS' macLib
extern crate libc;

use libc::{c_char, c_void, c_long};
use std::ffi::{CStr,CString};

#[repr(C)]
#[allow(non_snake_case)]
pub struct MAC_HANDLE { _unused: [u8; 0] }

#[allow(non_snake_case)]
extern "C" {
    fn macCreateHandle(
        handle: *mut *mut MAC_HANDLE,
        pairs: *const *const c_char
    ) -> c_long;

    fn macDeleteHandle(
        handle: *mut MAC_HANDLE
    ) -> c_long;

    fn macPutValue(
        handle: *mut MAC_HANDLE,
        name: *const c_char,
        value: *const c_char
    ) -> c_long;

    fn macPushScope(
        handle: *mut MAC_HANDLE
    ) -> c_long;

    fn macPopScope(
        handle: *mut MAC_HANDLE
    ) -> c_long;

    /*fn macReportMacros(
        handle: *mut MAC_HANDLE
    ) -> c_long;*/

    fn macParseDefns(
        handle: *mut MAC_HANDLE,
        defns: *const c_char,
        pairs: *mut *mut *mut c_char
    ) -> c_long;

    fn macInstallMacros(
        handle: *mut MAC_HANDLE,
        pairs: *const *const c_char
    ) -> c_long;

    fn macDefExpand(
        s: *const c_char,
        macros: *mut MAC_HANDLE
    ) -> *mut c_char;
}

// Flatten (name, value) pairs
fn flatten<T: AsRef<str>>(pairs: &[(T, T)]) -> (Vec<CString>, Vec<*const c_char>) {
    let mut flattened = Vec::with_capacity(pairs.len()*2);
    let mut flattened_ptrs = Vec::with_capacity(pairs.len()*2 + 2);

    for pair in pairs.iter() {
        let name = CString::new(pair.0.as_ref()).unwrap();
        let value = CString::new(pair.1.as_ref()).unwrap();

        flattened.push(name);
        flattened.push(value);
    }

    for s in flattened.iter() {
        flattened_ptrs.push(s.as_ptr())
    }

    flattened_ptrs.push(std::ptr::null());
    flattened_ptrs.push(std::ptr::null());

    (flattened, flattened_ptrs)
}

pub fn parse_defns(defns: &str) -> Result<Vec<(String, String)>,i64> {

    let defns = CString::new(defns).unwrap();
    let mut pairs: *mut *mut c_char = std::ptr::null_mut();
    let pairs_ptr: *mut *mut *mut c_char = &mut pairs;

    let r = unsafe {
        macParseDefns(std::ptr::null_mut(), defns.as_ptr(), pairs_ptr)
    };

    if r < 0 {
        return Err(r);
    }

    let mut result = Vec::new();

    let mut p : *mut c_char = unsafe { *pairs };

    while !p.is_null() {
        let name = unsafe { CStr::from_ptr(p) };
        let value = unsafe { CStr::from_ptr(p.offset(1)) };

        result.push((
            name.to_str().ok().unwrap().to_owned(),
            value.to_str().ok().unwrap().to_owned()
        ));

        p = unsafe { p.offset(2) }
    }

    unsafe {
        libc::free(pairs as *mut c_void);
    }

    Ok(result)
}

pub struct Mac {
    handle: *mut MAC_HANDLE
}

impl Drop for Mac {
    fn drop(&mut self) {
        unsafe {
            macDeleteHandle(self.handle);
        }
    }
}

impl Mac {

    pub fn new(pairs: &[(&str, &str)]) -> Result<Mac, i64> {
        let mut handle: *mut MAC_HANDLE = std::ptr::null_mut();
        let handle_ptr : *mut *mut MAC_HANDLE = &mut handle;

        let (_flattened, flattened_ptrs) = flatten(pairs);

        let r = unsafe {
            macCreateHandle(handle_ptr, flattened_ptrs.as_ptr())
        };

        if r < 0 {
            Err(r)
        } else {
            Ok(Mac {
                handle: handle
            })
        }
    }

    pub fn clear_value(&self, name: &str) {
        let name = CString::new(name).unwrap();

        unsafe {
            macPutValue(self.handle, name.as_ptr(), std::ptr::null());
        }
    }

    pub fn push_scope(&self) {
        unsafe {
            macPushScope(self.handle);
        }
    }

    pub fn pop_scope(&self) {
        unsafe {
            macPopScope(self.handle);
        }
    }

    /*pub fn report_macros(&self) {
        unsafe {
            macReportMacros(self.handle);
        }
    }*/

    pub fn install_macros<T: AsRef<str>>(&self, pairs: &[(T, T)]) {
        // Flatten pairs
        let (_flattened, flattened_ptrs) = flatten(pairs);

        unsafe {
            macInstallMacros(self.handle, flattened_ptrs.as_ptr());
        };

    }

    pub fn def_expand(&self, src: &str) -> Result<String,()> {
        let src = CString::new(src).unwrap();

        let expanded: *const c_char = unsafe {
            macDefExpand(src.as_ptr(), self.handle)
        };

        if expanded.is_null() {
            return Err(());
        }

        let result = unsafe { CStr::from_ptr(expanded) }.to_str().ok().unwrap();
        let result = result.to_owned();

        unsafe {
            libc::free(expanded as *mut c_void);
        }

        Ok(result)
    }
}
