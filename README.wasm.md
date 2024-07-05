# Building Flang as a WASM cross-compiler

From the branch ``feature/flang-wasm`` of
https://github.com/serge-sans-paille/llvm-project.

This work is inspired by https://gws.phd/posts/fortran_wasm/.

## Building the compiler itself

```
$ cmake -S llvm -B _fbuild -GNinja \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX=_finstall \
        -DLLVM_DEFAULT_TARGET_TRIPLE=wasm32-unknown-emscripten \
        -DLLVM_TARGETS_TO_BUILD=WebAssembly \
        -DLLVM_ENABLE_PROJECTS="clang;flang;mlir"
$ cmake --build _fbuild
$ cmake --build _fbuild --target install
```

## Building Flang runtmine

Assuming you have ``emcmake`` available somewhere in your ``PATH``.

```
$ emcmake cmake -S flang/runtime -B _fbuild_runtime -GNinja \
                -DCMAKE_BUILD_TYPE=Release \
                -DCMAKE_INSTALL_PREFIX=_finstall
$ cmake --build _fbuild_runtime
$ cmake --build _fbuild_runtime --target install
```
