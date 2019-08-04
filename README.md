# An implementation of the Kaleidoscope language using Kotlin/Native

To build the examples:
1) Install or download LLVM on your computer
2) Update `llvm.def` for your system and LLVM location
3) Run `./gradlew clean build`
4) Find required executable in `build/bin/kaleidoscope` 

This implementation uses c interface of LLVM.

Kotlin/Native: https://kotlinlang.org/docs/reference/native-overview.html  
LLVM: https://llvm.org/  
Kaleidoscope: https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html
