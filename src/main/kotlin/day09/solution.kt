package day09

import java.io.File

typealias Block = List<IndexedValue<Int?>>

data class FileSystem(val disk: List<Int?>) {
    private fun firstNullIndex(disk: List<Int?>) = disk.indexOfFirst { it == null }
    private fun lastNonNullIndex(disk: List<Int?>) = disk.indexOfLast { it != null }

    fun rearrange(): List<Int?> {
        val disk = disk.toMutableList()
        var firstNullIndex = firstNullIndex(disk)
        var lastNonNullIndex = lastNonNullIndex(disk)

        while (firstNullIndex < lastNonNullIndex) {
            val id = disk[lastNonNullIndex]
            disk[firstNullIndex] = id
            disk[lastNonNullIndex] = null
            firstNullIndex = firstNullIndex(disk)
            lastNonNullIndex = lastNonNullIndex(disk)
        }

        return disk
    }

    private fun groupAdjacent() = disk.withIndex()
        .fold(mutableListOf<MutableList<IndexedValue<Int?>>>()) { list, item ->
            list.apply {
                if (isEmpty() || last().last().value != item.value)
                    add(mutableListOf(item))
                else
                    last().add(item)
            }
        }

    fun rearrange2(): List<Int?> {
        val groups = groupAdjacent()
        val (freeGroups, occupiedGroups) = groups.partition { it.first().value == null }

        val swaps = mutableListOf<Pair<Block, Block>>()
        val taken = mutableSetOf<Int>()
        for (occupied in occupiedGroups.reversed()) {
            for (free in freeGroups) {
                if (free.last().index > occupied.first().index) {
                    break
                }

                val actuallyFree = free.filter { it.index !in taken }
                if (actuallyFree.size < occupied.size) {
                    continue
                }

                val toUse = actuallyFree.take(occupied.size)
                swaps.add(toUse to occupied)
                taken.addAll(toUse.map { it.index })
                break
            }
        }

        val newDisk = disk.toMutableList()
        for ((nulls, nonNulls) in swaps) {
            val id = nonNulls.first().value
            for (n in nulls) {
                newDisk[n.index] = id
            }
            for (nn in nonNulls) {
                newDisk[nn.index] = null
            }
        }

        return newDisk
    }
}

fun parse(inp: String): List<Int?> {
    val diskMap = inp.map { it.digitToInt() }
    var number = 0
    val disk = mutableListOf<Int?>()
    for ((id, count) in diskMap.withIndex()) {
        val toAdd = if (id % 2 == 0) number++ else null
        disk.addAll(List(count) { toAdd })
    }

    return disk
}

fun first(inp: String): Long {
    val fileSystem = FileSystem(parse(inp))

    return fileSystem.rearrange()
        .filterNotNull()
        .mapIndexed { index, id -> (index * id).toLong() }
        .sum()
}

fun second(inp: String): Long {
    val fileSystem = FileSystem(parse(inp))

    return fileSystem.rearrange2()
        .withIndex()
        .filter { it.value != null }
        .sumOf { (index, id) -> (index * id!!).toLong() }
}

fun main() {
    val testInput = File("src/main/resources/day09/test-input.txt").readText()
    val input = File("src/main/resources/day09/input.txt").readText()
    println(first(input))
    println(second(input))
}
