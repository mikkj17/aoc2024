package day02

import java.io.File
import kotlin.math.abs

fun parse(inp: String) = inp.lines()
    .map { line -> line.split(" ").map { it.toInt() } }

fun safe(levels: List<Int>): Boolean {
    return levels in setOf(levels.sorted(), levels.sortedDescending())
            && levels.zipWithNext().all { (a, b) -> abs(a - b) in 1..3 }
}

fun first(inp: String) = parse(inp)
    .filter { safe(it) }
    .size

fun generate(levels: List<Int>) = levels.indices
    .map { levels.take(it) + levels.drop(it + 1) }

fun second(inp: String) = parse(inp)
    .filter { level -> generate(level).any { safe(it) } }
    .size

fun main() {
    val testInput = File("src/main/kotlin/day02/test-input.txt").readText()
    val input = File("src/main/kotlin/day02/input.txt").readText()
    println(first(input))
    println(second(input))
}
