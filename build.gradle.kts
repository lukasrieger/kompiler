import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

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

    maven(url = "https://dl.bintray.com/arrow-kt/arrow-kt/")
    maven(url = "https://oss.jfrog.org/artifactory/oss-snapshot-local/" )
    maven(url = "https://dl.bintray.com/kotlin/kotlin-eap" )
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


    implementation("io.arrow-kt:arrow-fx:${Versions.arrowVersion}")
    implementation("io.arrow-kt:arrow-fx-coroutines:${Versions.arrowVersion}")
    implementation("io.arrow-kt:arrow-syntax:${Versions.arrowVersion}")
    implementation("io.arrow-kt:arrow-optics:${Versions.arrowVersion}")
    kapt ("io.arrow-kt:arrow-meta:${Versions.arrowVersion}")
}

val compileKotlin: KotlinCompile by tasks

tasks.withType<KotlinCompile>().configureEach {
    kotlinOptions.jvmTarget = "1.8"
    kotlinOptions.freeCompilerArgs += listOf("-Xinline-classes","-Xopt-in=kotlin.contracts.ExperimentalContracts")
}


tasks {
    test {
        useJUnitPlatform()
    }
}

application {
    mainClass.set("MainKt")
}
