package day10

import java.io.File

typealias Position = Pair<Int, Int>

const val top = 9

val directions = listOf(
    -1 to 0,
    1 to 0,
    0 to -1,
    0 to 1,
)

data class TopographicMap(val heights: List<List<Int>>) {
    fun heightOf(position: Position) = heights.getOrNull(position.first)
        ?.getOrNull(position.second) ?: -1

    fun getTrailHeads(): List<Position> {
        val heads = mutableListOf<Position>()

        for ((y, row) in heights.withIndex()) {
            for ((x, element) in row.withIndex()) {
                if (element == 0) {
                    heads.add(y to x)
                }
            }
        }

        return heads
    }

    fun neighboursOf(position: Position): List<Position> {
        val height = heightOf(position)
        return directions
            .map { (dy, dx) -> position.let { it.first + dy to it.second + dx } }
            .filter { heightOf(it) - height == 1 }
    }
}

fun parse(inp: String) = TopographicMap(inp.lines().map { line -> line.map { it.digitToInt() } })

fun compute(map: TopographicMap, search: (TopographicMap, Position) -> Int) = map
    .getTrailHeads()
    .sumOf { search(map, it) }

fun first(inp: String): Int = compute(parse(inp)) { map, trailhead ->
    val explored = mutableSetOf(trailhead)
    val queue = mutableListOf(trailhead)
    while (queue.isNotEmpty()) {
        val v = queue.removeFirst()
        for (w in map.neighboursOf(v)) {
            if (w !in explored) {
                explored += w
                queue += w
            }
        }
    }

    explored.count { map.heightOf(it) == top }
}

fun second(inp: String): Int = compute(parse(inp)) { map, trailhead ->
    fun search(position: Position): Int {
        if (map.heightOf(position) == top) {
            return 1
        }

        return map.neighboursOf(position)
            .sumOf { search(it) }
    }

    search(trailhead)
}

fun main() {
    val testInput = File("src/main/resources/day10/test-input.txt").readText()
    val input = File("src/main/resources/day10/input.txt").readText()
    println(first(input))
    println(second(input))
}
