# This is a Makefile fragment, see src/libCom/Makefile.

Com_LIBS += iocsh
DB += iocsh_test.db

IOCSH_RUST=$(LIBCOM)/iocsh-rust
IOCSH_RUST_TARGET=$(IOCSH_RUST)/target/debug

SRC_DIRS += $(IOCSH_RUST)

libiocsh.a: libiocsh.so

# This builds both .a and .so
libiocsh.so: $(addprefix $(IOCSH_RUST)/src/, lib.rs ffi.rs mac.rs iocsh.rs)
	cargo build --manifest-path=$(IOCSH_RUST)/Cargo.toml
	cp $(IOCSH_RUST_TARGET)/$@ $(CURDIR)/
	cp $(IOCSH_RUST_TARGET)/libiocsh.a $(CURDIR)/
