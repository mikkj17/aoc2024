package day04

import java.io.File

typealias Crossword = List<List<Char>>
typealias Position = Pair<Int, Int>

private val directions = listOf(
    -1 to -1,
    -1 to 0,
    -1 to 1,
    0 to -1,
    0 to 1,
    1 to -1,
    1 to 0,
    1 to 1,
)

fun compute(inp: String, search: (Crossword, Position) -> Int): Int {
    val crossword = inp.lines().map { it.toCharArray().toList() }
    return crossword.indices.sumOf { y ->
        crossword.first().indices.sumOf { x ->
            search(crossword, y to x)
        }
    }
}

fun Crossword.getByPosition(position: Position) = this
    .getOrNull(position.first)
    ?.getOrNull(position.second)

fun getWords(crossword: Crossword, fromPos: Position) = directions.map { (dy, dx) ->
    var (y, x) = fromPos
    List(4) {
        crossword.getByPosition(y to x).also {
            y += dy
            x += dx
        }
    }
        .filterNotNull()
        .joinToString("")
}

fun first(inp: String) = compute(inp) { crossword, pos ->
    getWords(crossword, pos).count { it == "XMAS" }
}

fun getDiagonalWords(crossword: Crossword, fromPos: Position) = (-1..1)
    .fold("" to "") { (a, b), i ->
        Pair(
            a + crossword.getByPosition(fromPos.first + i to fromPos.second + i),
            b + crossword.getByPosition(fromPos.first + i to fromPos.second - i),
        )
    }

fun second(inp: String) = compute(inp) { crossword, pos ->
    getDiagonalWords(crossword, pos).let { words ->
        if (words.toList().all { it in listOf("MAS", "SAM") }) 1
        else 0
    }
}

fun main() {
    val testInput = File("src/main/resources/day04/test-input.txt").readText()
    val input = File("src/main/resources/day04/input.txt").readText()
    println(first(input))
    println(second(input))
}
