package lab11

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.selects.selectUnbiased
import kotlin.random.Random

fun main() {
    val PHILOSOPHER_COUNT = 5
    // PHILOSOPHER_COUNT*2 fork channels (2 channels for every fork - left and right philosopher)
    val forkChannels = List(PHILOSOPHER_COUNT) { List(2) { Channel<Unit>() } }
    runBlocking {
        for (id in 0..<PHILOSOPHER_COUNT) {
            launch { philosopher(id+1, forkChannels[id][0],
                forkChannels[(id+1)%PHILOSOPHER_COUNT][1]) }
            launch { fork(forkChannels[id]) }
        }
    }
}

private suspend fun philosopher(id: Int, leftFork: Channel<Unit>, rightFork: Channel<Unit>) {
    while (true) {
        delay(Random.nextLong(200))
        leftFork.send(Unit)
        println("Philosopher $id got left fork")
        rightFork.send(Unit)
        println("Philosopher $id got right fork")
        delay(Random.nextLong(200))
        println("Philosopher $id ate")
        rightFork.send(Unit)
        leftFork.send(Unit)
    }
}

private suspend fun fork(channels: List<Channel<Unit>>) {
    while (true) {
        selectUnbiased {
            for (channel in channels) {
                channel.onReceive {
                    channel.receive()
                }
            }
        }
    }
}
