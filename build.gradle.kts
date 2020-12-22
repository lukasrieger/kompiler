import org.jetbrains.kotlin.gradle.tasks.KotlinCompile


// a small hack: the variable must be named like the property
// jitpack will pass -Pversion=..., so `val version` is required here.
val version: String by project
// we create an alias here...
val versionProperty = version
// do the same for group
val group: String by project
val groupProperty = if (group.endsWith(".antlr-kotlin")) {
    group
} else {
    // just another jitpack hack
    "$group.antlr-kotlin"
}

//val antlrVersion = "4.7.1"
val antlrKotlinVersion = versionProperty
// you can also use a jitpack version:
//val antlrKotlinVersion = "86a86f1968"

buildscript {
    // we have to re-declare this here :-(

    // a small hack: the variable must be named like the property
    // jitpack will pass -Pversion=..., so `val version` is required here.
    val version: String by project
    // we create an alias here...
    val versionProperty = version
    // do the same for group
    val group: String by project
    val groupProperty = if (group.endsWith(".antlr-kotlin")) {
        group
    } else {
        // just another jitpack hack
        "$group.antlr-kotlin"
    }

    val antlrKotlinVersion = versionProperty
    // you can also use a jitpack version (we have to re-declare this here):
    //val antlrKotlinVersion = "86a86f1968"

    dependencies {
        // add the plugin to the classpath
        classpath("$groupProperty:antlr-kotlin-gradle-plugin:$antlrKotlinVersion")
    }
}

plugins {
    kotlin("jvm") version "1.4.20"
    kotlin("kapt") version "1.4.20"
    id("org.jetbrains.dokka") version "1.4.20"
    id("application")
    `maven-publish`
}


object Versions {
    const val arrowVersion = "0.12.0-SNAPSHOT"
    const val mockkVersion = "1.10.0"
}


repositories {
    mavenCentral()
    jcenter()
    mavenLocal()

    maven(url = "https://dl.bintray.com/arrow-kt/arrow-kt/")
    maven(url = "https://oss.jfrog.org/artifactory/oss-snapshot-local/")
    maven(url = "https://dl.bintray.com/kotlin/kotlin-eap")

    maven("https://jitpack.io")
}

tasks.register<com.strumenta.antlrkotlin.gradleplugin.AntlrKotlinTask>("generateKotlinGrammarSource") {
    // the classpath used to run antlr code generation
    antlrClasspath = configurations.detachedConfiguration(
        // antlr itself
        // antlr is transitive added by antlr-kotlin-target,
        // add another dependency if you want to choose another antlr4 version (not recommended)
        // project.dependencies.create("org.antlr:antlr4:$antlrVersion"),

        // antlr target, required to create kotlin code
        project.dependencies.create("$groupProperty:antlr-kotlin-target:$antlrKotlinVersion")
    )
    maxHeapSize = "64m"
    packageName = "com.strumenta.antlrkotlin.examples"
    arguments = listOf("-no-visitor", "-no-listener")
    source = project.objects
        .sourceDirectorySet("antlr", "antlr")
        .srcDir("src/main/antlr").apply {
            include("*.g4")
        }
    // outputDirectory is required, put it into the build directory
    // if you do not want to add the generated sources to version control
    outputDirectory = File("build/generated-src/antlr/main")
    // use this settings if you want to add the generated sources to version control
    // outputDirectory = File("src/main/kotlin-antlr")
}

// run generate task before build
// not required if you add the generated sources to version control
// you can call the task manually in this case to update the generated sources
tasks.getByName("compileKotlin").dependsOn("generateKotlinGrammarSource")

// you have to add the generated sources to kotlin compiler source directory list
configure<SourceSetContainer> {
    named("main") {
        withConvention(org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet::class) {
            kotlin.srcDir("build/generated-src/antlr/main")
            // kotlin.srcDir("src/main/kotlin-antlr")
        }
    }
}



dependencies {
    testImplementation("io.kotest:kotest-runner-junit5:4.2.3")
    testImplementation("io.kotest:kotest-assertions-core:4.2.3")
    testImplementation("io.kotest:kotest-property:4.2.3")
    testImplementation("io.mockk:mockk:${Versions.mockkVersion}")
    testImplementation("junit:junit:4.13")


    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    implementation("org.jetbrains.kotlin:kotlin-reflect:1.4.0")


    implementation("org.apache.logging.log4j:log4j-core:2.13.3")
    implementation("$groupProperty:antlr-kotlin-runtime-jvm:$antlrKotlinVersion")


    implementation("io.arrow-kt:arrow-fx:${Versions.arrowVersion}")
    implementation("io.arrow-kt:arrow-fx-coroutines:${Versions.arrowVersion}")
    implementation("io.arrow-kt:arrow-syntax:${Versions.arrowVersion}")
    implementation("io.arrow-kt:arrow-optics:${Versions.arrowVersion}")
    implementation("io.arrow-kt:arrow-mtl:${Versions.arrowVersion}")
    implementation("io.arrow-kt:arrow-mtl-data:${Versions.arrowVersion}")
    kapt("io.arrow-kt:arrow-meta:${Versions.arrowVersion}")
}

val compileKotlin: KotlinCompile by tasks

tasks.withType<KotlinCompile>().configureEach {
    kotlinOptions.jvmTarget = "1.8"
    kotlinOptions.freeCompilerArgs += listOf(
        "-Xinline-classes",
        "-Xopt-in=kotlin.contracts.ExperimentalContracts",
        "-Xopt-in=kotlin.RequiresOptIn"
    )
}


tasks {
    test {
        useJUnitPlatform()
    }
}

application {
    mainClass.set("MainKt")
}
