package day00

import java.io.File

fun first(inp: String): Int {
    throw NotImplementedError()
}

fun second(inp: String): Int {
    throw NotImplementedError()
}

fun main() {
    val testInput = File("src/main/kotlin/day00/test-input.txt").readText()
    val input = File("src/main/kotlin/day00/input.txt").readText()
    println(first(testInput))
    println(second(testInput))
}
