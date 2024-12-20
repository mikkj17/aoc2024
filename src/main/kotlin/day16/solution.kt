package day16

import java.io.File
import java.util.*

typealias Position = Pair<Int, Int>
typealias Direction = Pair<Int, Int>

fun Position.step(direction: Direction) = first + direction.first to second + direction.second

val directions = listOf(-1 to 0, 1 to 0, 0 to 1, 0 to -1)

data class State(val position: Position, val direction: Direction)

data class Maze(val start: Position, val end: Position, val walls: Set<Position>) {
    fun neighbors(state: State) = directions
        .filter { direction -> direction != state.direction.let { it.first * (-1) to it.second * (-1) } }
        .map { State(state.position.step(it), it) }
        .filter { it.position !in walls }

    fun cost(current: State, new: State) = when (new.direction) {
        current.direction -> 1
        else -> 1001
    }
}

fun parse(inp: String): Maze {
    lateinit var start: Position
    lateinit var end: Position
    val walls = mutableSetOf<Position>()

    for ((y, row) in inp.lines().withIndex()) {
        for ((x, value) in row.withIndex()) {
            val position = y to x
            when (value) {
                'S' -> start = position
                'E' -> end = position
                '#' -> walls += position
            }
        }
    }

    return Maze(start, end, walls)
}

fun compute(inp: String, search: Maze.() -> Int) = parse(inp).search()

fun first(inp: String) = compute(inp) {
    val queue = PriorityQueue<Pair<Int, State>>(compareBy { it.first })
    val startState = State(start, 0 to 1)
    queue.add(0 to startState)
    val expanded = mutableSetOf<Position>()

    while (queue.isNotEmpty()) {
        val (cost, state) = queue.poll()
        if (state.position == end) {
            return@compute cost
        }

        expanded += state.position
        for (neighbor in neighbors(state)) {
            if (neighbor.position !in expanded)
                queue.add(cost + cost(state, neighbor) to neighbor)
        }
    }

    throw AssertionError()
}

fun second(inp: String) = compute(inp) {
    val startState = State(start, 0 to 1)
    val costs = mutableMapOf(startState to 0)
    val predecessors = mutableMapOf<State, MutableList<State>>()
    val queue = mutableListOf(startState)

    while (queue.isNotEmpty()) {
        val state = queue.removeFirst()
        for (neighbor in neighbors(state)) {
            val currentCost = costs.getOrDefault(neighbor, Int.MAX_VALUE)
            val newCost = costs.getValue(state) + cost(state, neighbor)
            if (newCost < currentCost) {
                costs[neighbor] = newCost
                predecessors[neighbor] = mutableListOf(state)
                queue.add(neighbor)
            } else if (newCost == currentCost) {
                predecessors.getOrPut(neighbor) { mutableListOf() }.add(state)
            }
        }
    }

    val paths = mutableListOf<List<State>>()
    fun backTrack(state: State, path: List<State>) {
        if (state == startState) {
            paths.add((path + startState).reversed())
            return
        }

        for (predecessor in predecessors.getValue(state)) {
            backTrack(predecessor, path + state)
        }
    }

    val endState = costs.filter { it.key.position == end }.minBy { it.value }.key
    backTrack(endState, emptyList())

    return@compute paths.flatten().distinctBy { it.position }.count()
}

fun main() {
    val testInput = File("src/main/resources/day16/test-input.txt").readText()
    val testInput2 = File("src/main/resources/day16/test-input2.txt").readText()
    val input = File("src/main/resources/day16/input.txt").readText()
    println(first(input))
    println(second(input))
}
