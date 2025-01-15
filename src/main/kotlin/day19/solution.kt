package day19

import java.io.File

fun parse(inp: String): Pair<Set<String>, List<String>> {
    val (towels, designs) = inp.split("\n\n")
    return towels.split(", ").toSet() to designs.lines()
}

fun first(inp: String): Int {
    val (towels, designs) = parse(inp)
    fun possible(rest: String): Boolean {
        if (rest.isEmpty()) {
            return true
        }

        val prefixes = towels.filter { rest.startsWith(it) }
        if (prefixes.isEmpty()) {
            return false
        }

        return prefixes.any { possible(rest.drop(it.length)) }
    }
    return designs.count { possible(it) }
}

fun second(inp: String): Long {
    val (towels, designs) = parse(inp)

    val cache = mutableMapOf<String, Long>()
    fun findArrangements(rest: String): Long = cache.getOrPut(rest) {
        if (rest.isEmpty())
            1
        else
            towels.filter { rest.startsWith(it) }.sumOf { findArrangements(rest.drop(it.length)) }
    }

    return designs.sumOf { findArrangements(it) }
}

fun main() {
    val testInput = File("src/main/resources/day19/test-input.txt").readText()
    val input = File("src/main/resources/day19/input.txt").readText()
    println(first(input))
    println(second(input))
}
