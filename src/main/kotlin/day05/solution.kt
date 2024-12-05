package day05

import java.io.File

typealias Rule = Pair<Int, Int>
typealias Update = List<Int>

fun parse(inp: String): Pair<List<Rule>, List<Update>> {
    val (rules, updates) = inp.split("\n\n")
    return Pair(
        rules.lines().map { line -> line.split("|").map { it.toInt() }.let { it[0] to it[1] } },
        updates.lines().map { line -> line.split(",").map { it.toInt() } },
    )
}

fun Update.indexOfOrDefault(element: Int, default: Int) = this.indexOf(element).let {
    if (it == -1) default else it
}

fun Update.correctlyOrdered(rules: List<Pair<Int, Int>>) = rules.all { (first, second) ->
    this.indexOf(first) < this.indexOfOrDefault(second, this.size)
}

fun first(inp: String): Int {
    val (rules, updates) = parse(inp)
    return updates
        .filter { it.correctlyOrdered(rules) }
        .sumOf { it[it.size / 2] }
}

fun second(inp: String): Int {
    val (rules, updates) = parse(inp)
    return updates
        .filterNot { it.correctlyOrdered(rules) }
        .map { it.toMutableList() }
        .onEach {
            do {
                rules
                    .firstOrNull { (first, second) ->
                        it.indexOf(first) >= it.indexOfOrDefault(second, it.size)
                    }
                    ?.let { (first, second) ->
                        (it.indexOf(first) to it.indexOf(second)).apply {
                            it[this.first] = second
                            it[this.second] = first
                        }
                    }
            } while (!it.correctlyOrdered(rules))
        }
        .sumOf { it[it.size / 2] }
}

fun main() {
    val testInput = File("src/main/resources/day05/test-input.txt").readText()
    val input = File("src/main/resources/day05/input.txt").readText()
    println(first(input))
    println(second(input))
}
