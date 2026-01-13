package lab11

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.selects.selectUnbiased
import kotlin.random.Random

fun main() {
    val PHILOSOPHER_COUNT = 5
    // PHILOSOPHER_COUNT*2 fork channels (2 channels for every fork - left and right philosopher)
    val forkChannels = List(PHILOSOPHER_COUNT) { List(2) { Channel<Unit>() } }
    // PHILOSOPHER_COUNT awaiting channels
    val awaitChannels = List(PHILOSOPHER_COUNT) { Channel<Unit>() }
    // PHILOSOPHER_COUNT left channels
    val leftChannels = List(PHILOSOPHER_COUNT) { Channel<Unit>() }
    runBlocking {
        for (id in 0..<PHILOSOPHER_COUNT) {
            launch { philosopher(id+1, forkChannels[id][0],
                forkChannels[(id+1)%PHILOSOPHER_COUNT][1],
                awaitChannels[id], leftChannels[id]) }
            launch { fork(forkChannels[id]) }
        }
        launch { waiter(awaitChannels, leftChannels) }
    }
}

private suspend fun philosopher(id: Int, leftFork: Channel<Unit>, rightFork: Channel<Unit>,
                                awaitChannel: Channel<Unit>, leftChannel: Channel<Unit>) {
    while (true) {
        delay(Random.nextLong(200))
        awaitChannel.send(Unit)
        println("Philosopher $id got permission from waiter")
        leftFork.send(Unit)
        println("Philosopher $id got left fork")
        rightFork.send(Unit)
        println("Philosopher $id got right fork")
        delay(Random.nextLong(2000))
        println("Philosopher $id ate")
        rightFork.send(Unit)
        leftFork.send(Unit)
        leftChannel.send(Unit)
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

private suspend fun waiter(awaitChannels: List<Channel<Unit>>, leftChannels: List<Channel<Unit>>) {
    var freeSpace = awaitChannels.size - 1
    while (true) {
        selectUnbiased {
            for (channel in awaitChannels) {
                if (freeSpace > 0) {
                    channel.onReceive {
                        freeSpace--
                    }
                }
            }
            for (channel in leftChannels) {
                channel.onReceive {
                    freeSpace++
                }
            }
        }
    }
}
