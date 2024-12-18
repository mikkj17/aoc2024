package day15

import java.io.File
import kotlin.math.abs

typealias Position = Pair<Int, Int>
typealias Direction = Pair<Int, Int>

fun Position.move(direction: Direction) = Pair(
    first + direction.first,
    second + direction.second,
)

data class Warehouse(val robot: Robot, val boxes: List<Box>, val walls: List<Wall>) {
    private val height = walls.maxOf { it.height }
    private val width = walls.maxOf { it.width }

    override fun toString() = buildString {
        for (y in 0..height) {
            for (x in 0..width) {
                val pos = y to x
                append(
                    when {
                        pos == robot.position -> '@'
                        boxes.any { it.matches(pos) } -> boxes.first { it.matches(pos) }.toChar(pos)
                        walls.any { it.matches(pos) } -> walls.first { it.matches(pos) }.toChar(pos)
                        else -> '.'
                    }
                )
            }
            appendLine()
        }
    }

    fun move(direction: Direction) {
        var canMove = true
        val moves = mutableSetOf<Triple<Box, Position, Direction>>()

        fun simulateMoves(position: Position, direction: Direction) {
            // we hit a wall - cannot move
            if (walls.any { it.matches(position) }) {
                canMove = false
                return
            }

            // if we don't find a box, it means the spot is empty,
            // so we can stop recursing and move all boxes and robot
            val box = boxes.firstOrNull { it.matches(position) } ?: return

            // recursively checks if we can make the move in `direction`
            val nextPositions = box.prepareMove(position, direction)
            moves += nextPositions.map { Triple(box, position, direction) }
            nextPositions.forEach { simulateMoves(it, direction) }
        }

        val nextPosition = robot.position.move(direction)
        simulateMoves(nextPosition, direction)
        if (canMove) {
            robot.position = nextPosition
            moves.forEach { it.first.move(it.second, it.third) }
        }
    }
}

data class Robot(var position: Position)

interface Box {
    fun coordinate(): Int
    fun matches(position: Position): Boolean
    fun prepareMove(position: Position, direction: Direction): List<Position>
    fun move(from: Position, direction: Direction)
    fun toChar(position: Position): Char
}

interface Wall {
    val height: Int
    val width: Int
    fun matches(position: Position): Boolean
    fun toChar(position: Position): Char
}

data class SmallBox(var position: Position) : Box {
    override fun coordinate() = position.first * 100 + position.second

    override fun matches(position: Position) = this.position == position

    override fun prepareMove(position: Position, direction: Direction) = listOf(position.move(direction))

    override fun move(from: Position, direction: Direction) {
        if (from != position) {
            return
        }
        position = position.move(direction)
    }

    override fun toChar(position: Position) = 'O'
}

data class SmallWall(val position: Position) : Wall {
    override val height = position.first
    override val width = position.second
    override fun matches(position: Position) = this.position == position
    override fun toChar(position: Position) = '#'
}

data class WideBox(var left: Position, var right: Position) : Box {
    override fun coordinate() = left.first * 100 + left.second

    override fun matches(position: Position) = position in listOf(left, right)

    override fun prepareMove(position: Position, direction: Direction): List<Pair<Int, Int>> {
        val positions = listOf(left, right)
        if (abs(direction.second) == 1) {
            return positions.filter { it == position }.map { it.move(direction) }
        }
        return positions.map { it.move(direction) }
    }

    override fun move(from: Position, direction: Direction) {
        if (direction.second == -1 && from != left) {
            return
        }

        if (direction.second == 1 && from != right) {
            return
        }

        if (abs(direction.first) == 1 && from.first != left.first) {
            return
        }

        left = left.move(direction)
        right = right.move(direction)
    }

    override fun toChar(position: Position) = when (position) {
        left -> '['
        right -> ']'
        else -> throw AssertionError()
    }
}

data class WideWall(val left: Position, val right: Position) : Wall {
    override val height = left.first
    override val width = right.second

    override fun matches(position: Position) = position in listOf(left, right)
    override fun toChar(position: Position) = '#'
}

fun Char.toStep(): Direction = when (this) {
    '^' -> -1 to 0
    'v' -> 1 to 0
    '>' -> 0 to 1
    '<' -> 0 to -1
    else -> throw AssertionError()
}

fun parse(inp: String): Pair<Warehouse, List<Direction>> {
    lateinit var robot: Robot
    val boxes = mutableListOf<SmallBox>()
    val walls = mutableListOf<SmallWall>()

    val (warehouse, steps) = inp.split("\n\n")

    for ((y, row) in warehouse.lines().withIndex()) {
        for ((x, elem) in row.withIndex()) {
            val pos = y to x
            when (elem) {
                '@' -> robot = Robot(pos)
                'O' -> boxes += SmallBox(pos)
                '#' -> walls += SmallWall(pos)
            }
        }
    }

    return Pair(
        Warehouse(robot, boxes, walls),
        steps.replace("\n", "").map { it.toStep() }
    )
}

fun compute(warehouse: Warehouse, steps: List<Direction>): Int {
    steps.forEach { warehouse.move(it) }
    return warehouse.boxes.sumOf { it.coordinate() }
}

fun first(inp: String): Int {
    val (warehouse, steps) = parse(inp)
    return compute(warehouse, steps)
}

fun parseSecond(inp: String): Pair<Warehouse, List<Direction>> {
    lateinit var robot: Robot
    val boxes = mutableListOf<WideBox>()
    val walls = mutableListOf<WideWall>()

    val (warehouse, steps) = inp
        .replace("#", "()")
        .replace("O", "[]")
        .replace(".", "..")
        .replace("@", "@.")
        .split("\n\n")

    for ((y, row) in warehouse.lines().withIndex()) {
        for ((x, elem) in row.withIndex()) {
            val pos = y to x
            when (elem) {
                '@' -> robot = Robot(pos)
                '[' -> boxes += WideBox(pos, pos.let { it.first to it.second + 1 })
                '(' -> walls += WideWall(pos, pos.let { it.first to it.second + 1 })
            }
        }
    }

    return Pair(
        Warehouse(robot, boxes, walls),
        steps.replace("\n", "").map { it.toStep() }
    )
}

fun second(inp: String): Int {
    val (warehouse, steps) = parseSecond(inp)
    return compute(warehouse, steps)
}

fun main() {
    val testInput = File("src/main/resources/day15/test-input.txt").readText()
    val testInput2 = File("src/main/resources/day15/test-input2.txt").readText()
    val testInput3 = File("src/main/resources/day15/test-input3.txt").readText()
    val input = File("src/main/resources/day15/input.txt").readText()
    println(first(input))
    println(second(input))
}
