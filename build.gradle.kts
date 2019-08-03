plugins {
    kotlin("multiplatform") version "1.3.41"
}

group = "ru.alexpl"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

kotlin {

    // Determine host preset.
    val hostOs = System.getProperty("os.name")

    // Create target for the host platform.
    val hostTarget = when {
        hostOs == "Mac OS X" -> macosX64("kaleidoscope")
        hostOs == "Linux" -> linuxX64("kaleidoscope")
        hostOs.startsWith("Windows") -> mingwX64("kaleidoscope")
        else -> throw GradleException("Host OS '$hostOs' is not supported in Kotlin/Native $project.")
    }

    hostTarget.apply {
        val main by compilations.getting
        val llvm by main.cinterops.creating

        binaries {
            executable("ch2") {
                entryPoint = "ch2.main"
                runTask?.standardInput = System.`in`
            }
            executable("ch3") {
                entryPoint = "ch3.main"
                runTask?.standardInput = System.`in`
            }
            executable("ch4") {
                entryPoint = "ch4.main"
                runTask?.standardInput = System.`in`
            }
            executable("ch5") {
                entryPoint = "ch5.main"
                runTask?.standardInput = System.`in`
            }
            executable("ch6") {
                entryPoint = "ch6.main"
                runTask?.standardInput = System.`in`
            }
            executable("ch7") {
                entryPoint = "ch7.main"
                runTask?.standardInput = System.`in`
            }
        }
    }
}
