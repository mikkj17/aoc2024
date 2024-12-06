package day06

import java.io.File

typealias Grid = List<List<Char>>
typealias Position = Pair<Int, Int>
typealias Direction = Pair<Int, Int>

fun findStart(grid: Grid): Position {
    for ((y, row) in grid.withIndex()) {
        for ((x, elem) in row.withIndex()) {
            if (elem == '^') {
                return y to x
            }
        }
    }

    throw AssertionError()
}

fun search(grid: Grid, startPos: Position, onCycle: () -> Unit): Int {
    var position = startPos
    var direction = -1 to 0
    val visited = mutableSetOf<Pair<Position, Direction>>()
    while (true) {
        if (Pair(position, direction) in visited) {
            onCycle()
            break
        }

        visited.add(position to direction)
        val nextPosition = position.first + direction.first to position.second + direction.second
        val nextElement = grid.getOrNull(nextPosition.first)?.getOrNull(nextPosition.second) ?: break
        if (nextElement == '#') {
            direction = direction.let { it.second to it.first * -1 }
        } else {
            position = nextPosition
        }
    }

    return visited.distinctBy { it.first }.size
}

fun first(inp: String): Int {
    val grid = inp.lines().map { it.toCharArray().toList() }
    return search(grid, findStart(grid)) { }
}

fun second(inp: String): Int {
    val grid = inp.lines().map { it.toCharArray().toMutableList() }
    val startPos = findStart(grid)

    var count = 0
    for (row in grid) {
        for ((i, elem) in row.withIndex()) {
            if (elem != '.') continue
            row[i] = '#'
            search(grid, startPos) { count++ }
            row[i] = '.'
        }
    }

    return count
}

fun main() {
    val testInput = File("src/main/resources/day06/test-input.txt").readText()
    val input = File("src/main/resources/day06/input.txt").readText()
    println(first(input))
    println(second(input))
}
