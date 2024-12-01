package day01

import java.io.File
import kotlin.math.abs

fun parse(inp: String) = inp.lineSequence()
    .map { line -> line.split("   ").map { it.toInt() }.let { it[0] to it[1] } }
    .unzip()

fun first(inp: String): Int {
    val (left, right) = parse(inp)
    return left.sorted()
        .zip(right.sorted()) { a, b -> abs(a - b) }
        .sum()
}

fun second(inp: String): Int {
    val (left, right) = parse(inp)
    return left.sumOf { num -> num * right.count { it == num } }
}

fun main() {
    val testInput = File("src/main/kotlin/day01/test-input.txt").readText()
    val input = File("src/main/kotlin/day01/input.txt").readText()
    println(first(input))
    println(second(input))
}
