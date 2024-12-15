package day12

import java.io.File
import kotlin.math.abs

typealias Grid = List<List<Char>>
typealias Position = Pair<Int, Int>
typealias Direction = Pair<Int, Int>

fun Grid.getAtPosition(position: Position) = getOrNull(position.first)?.getOrNull(position.second)
fun Position.step(direction: Direction) = first + direction.first to second + direction.second

val directions = listOf(
    -1 to 0,
    1 to 0,
    0 to -1,
    0 to 1,
)

data class Region(val name: Char, private val positions: Set<Position>) {
    fun area() = positions.size

    fun perimeter() = positions.sumOf { position ->
        directions.count { position.step(it) !in positions }
    }

    fun sides() = positions.asSequence().flatMap { Corner.from(it) }.toSet()
        .map { corner -> corner.positions.filter { it in positions } }
        .map { positions ->
            when (positions.size) {
                1 -> 1
                2 -> {
                    val (a, b) = positions
                    if (abs(a.first - b.first) to abs(a.second - b.second) == 1 to 1) 2
                    else 0
                }

                3 -> 1
                else -> 0
            }
        }
        .sum()
}

data class Corner(val positions: List<Position>) {
    companion object {
        fun from(position: Position): List<Corner> {
            return listOf(
                listOf(0 to 0, 0 to -1, -1 to -1, -1 to 0), // North-west
                listOf(0 to 0, -1 to 0, -1 to 1, 0 to 1),   // North-east
                listOf(0 to 0, 0 to 1, 1 to 1, 1 to 0),     // South-east
                listOf(0 to 0, 1 to 0, 1 to -1, 0 to -1),   // South-west
            )
                .map { dirs ->
                    val positions = dirs.map { position.step(it) }
                        .sortedWith(compareBy<Position> { it.first }.thenBy { it.second })
                    Corner(positions)
                }
        }
    }
}

fun discover(grid: Grid, start: Position): Set<Position> {
    val name = grid.getAtPosition(start)
    val discovered = mutableSetOf<Position>()
    fun search(position: Position): Set<Position> {
        discovered += position
        directions.map { position.step(it) }.forEach {
            if (it !in discovered && grid.getAtPosition(it) == name) {
                search(it)
            }
        }

        return discovered
    }

    return search(start)
}

fun parse(inp: String): List<Region> {
    val grid = inp.lines().map { it.toList() }
    val regions = mutableListOf<Region>()

    val visited = mutableSetOf<Position>()
    for ((y, row) in grid.withIndex()) {
        for ((x, name) in row.withIndex()) {
            if (y to x !in visited) {
                val positions = discover(grid, y to x)
                regions.add(Region(name, positions))
                visited += positions
            }
        }
    }

    return regions
}

fun compute(inp: String, cost: (Region) -> Int) = parse(inp).sumOf(cost)

fun first(inp: String) = compute(inp) { it.area() * it.perimeter() }

fun second(inp: String) = compute(inp) { it.area() * it.sides() }

fun main() {
    val testInput = File("src/main/resources/day12/test-input.txt").readText()
    val testInput2 = File("src/main/resources/day12/test-input2.txt").readText()
    val testInput3 = File("src/main/resources/day12/test-input3.txt").readText()
    val input = File("src/main/resources/day12/input.txt").readText()
    println(first(input))
    println(second(input))
}
