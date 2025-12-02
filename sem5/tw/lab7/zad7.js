let Fork = function() {
    this.state = 0;
    return this;
}

Fork.prototype.acquire = async function() { 
    // zaimplementuj funkcje acquire, tak by korzystala z algorytmu BEB
    // (http://pl.wikipedia.org/wiki/Binary_Exponential_Backoff), tzn:
    // 1. przed pierwsza proba podniesienia widelca Filozof odczekuje 1ms
    // 2. gdy proba jest nieudana, zwieksza czas oczekiwania dwukrotnie
    //    i ponawia probe itd.
	let totalWait = 0;
	const attempt = (res, waitTime) => {
		totalWait += waitTime;
		setTimeout(() => {
			if (this.state == 0) {
				this.state = 1;
				res(totalWait);
			}
			else attempt(res, Math.min(waitTime*2, 1024));
		}, waitTime);
	}
	return new Promise((res) => { attempt(res, 1) });
}

Fork.prototype.release = function() { 
	this.state = 0;
}

let Philosopher = function(id, forks) {
    this.id = id;
    this.forks = forks;
    this.f1 = id % forks.length;
    this.f2 = (id+1) % forks.length;
    return this;
}

Philosopher.prototype.sleep = function(minTime=100, maxTime=200) {
	let diff = maxTime - minTime + 1;
	let sleepTime = minTime + parseInt(Math.random() * diff);
	return new Promise((res) => { setTimeout(() => { res() }, sleepTime) });
}

Philosopher.prototype.startNaive = async function(count) {
    let forks = this.forks,
        f1 = this.f1,
        f2 = this.f2,
        id = this.id;
    
    // zaimplementuj rozwiazanie naiwne
    // kazdy filozof powinien 'count' razy wykonywac cykl
    // podnoszenia widelcow -- jedzenia -- zwalniania widelcow
	for (; count > 0; count--) {
		await this.sleep();
		await forks[f1].acquire();
		console.log("Philosopher " + id + " acquired fork (" + f1 + ")");
		await forks[f2].acquire();
		console.log("Philosopher " + id + " acquired fork (" + f2 + ")");
		await this.sleep();
		console.log("Philosopher " + id + " ate");
		forks[f1].release();
		forks[f2].release();
	}
}

Philosopher.prototype.startAsym = async function(count) {
    let forks = this.forks,
        f1 = this.f1,
        f2 = this.f2,
        id = this.id;
    
    // zaimplementuj rozwiazanie asymetryczne
    // kazdy filozof powinien 'count' razy wykonywac cykl
    // podnoszenia widelcow -- jedzenia -- zwalniania widelcow
	if (this.id % 2) {
		f1 = this.f2;
		f2 = this.f1;
	}
	let times = [];
	for (; count > 0; count--) {
		await this.sleep();
		times.push(await forks[f1].acquire());
		console.log("Philosopher " + id + " acquired fork (" + f1 + ")");
		times.push(await forks[f2].acquire());
		console.log("Philosopher " + id + " acquired fork (" + f2 + ")");
		await this.sleep();
		console.log("Philosopher " + id + " ate");
		forks[f1].release();
		forks[f2].release();
	}
	return new Promise((res) => res(times));
}

let Waiter = function(total) {
    this.state = 0;
	this.total = total;
    return this;
}

Waiter.prototype.acquire = async function() { 
	let totalWait = 0;
	const attempt = (res, waitTime) => {
		totalWait += waitTime;
		setTimeout(() => {
			if (this.state < this.total-1) {
				this.state++;
				res(totalWait);
			}
			else attempt(res, Math.min(waitTime*2, 1024));
		}, waitTime);
	}
	return new Promise((res) => { attempt(res, 1) });
}

Waiter.prototype.release = function() { 
	this.state -= 1;
}

Philosopher.prototype.startConductor = async function(count, waiter) {
    let forks = this.forks,
        f1 = this.f1,
        f2 = this.f2,
        id = this.id;
    
    // zaimplementuj rozwiazanie z kelnerem
    // kazdy filozof powinien 'count' razy wykonywac cykl
    // podnoszenia widelcow -- jedzenia -- zwalniania widelcow
	let timesForks = [];
	let timesWaiter = [];
	for (; count > 0; count--) {
		await this.sleep();
		timesWaiter.push(await waiter.acquire());
		console.log("Philosopher " + id + " got permission from waiter");
		timesForks.push(await forks[f1].acquire());
		console.log("Philosopher " + id + " acquired fork (" + f1 + ")");
		timesForks.push(await forks[f2].acquire());
		console.log("Philosopher " + id + " acquired fork (" + f2 + ")");
		await this.sleep();
		console.log("Philosopher " + id + " ate");
		forks[f1].release();
		forks[f2].release();
		waiter.release();
	}
	return new Promise((res) => res([timesForks, timesWaiter]));
}

function run(version) {
	let count = 10;
	let N = 5;
	let forks = [];
	let philosophers = []
	for (let i = 0; i < N; i++) {
		forks.push(new Fork());
	}

	for (let i = 0; i < N; i++) {
		philosophers.push(new Philosopher(i, forks));
	}

	let fun = null;
	switch (version) {
		case 0:
			fun = (i) => philosophers[i].startNaive(count);
			break;
		case 1:
			fun = (i) => philosophers[i].startAsym(count);
			break;
		case 2:
			let waiter = new Waiter(N);
			fun = (i) => philosophers[i].startConductor(count, waiter);
			break;
	}

	for (let i = 0; i < N; i++) {
		fun(i);
	}
}

import { open } from 'node:fs/promises';
async function writeToFile(filePath, option, N, times) {
	let handle;
	try {
		handle = await open(filePath, option);
		for (let t of times) {
			handle.appendFile(`${N},${t}\n`);
		}
	} finally {
		await handle?.close();
	}
}

async function test(minN, maxN) {
	let count = 10;
	let asymFile = "asym.csv"
	let waiterForksFile = "waiter_forks.csv"
	let waiterWaiterFile = "waiter_waiter.csv"
//	for (let f of [asymFile, waiterForksFile, waiterWaiterFile])
//		await writeToFile(f, "w", "N", ["time"])

	for (let N = minN; N <= maxN; N++) {
		console.log(`${(new Date()).getHours()}:${(new Date()).getMinutes()} - testing N=${N}`)
		let forks = [];
		let philosophers = []
		let waiter = new Waiter(N);
		let timesAsym = [];
		let timesWaiterForks = [];
		let timesWaiterWaiter = [];

		for (let i = 0; i < N; i++) {
			forks.push(new Fork());
		}
		for (let i = 0; i < N; i++) {
			philosophers.push(new Philosopher(i, forks));
		}

		let promises = [];
		for (let i = 0; i < N; i++) {
			promises.push(philosophers[i].startAsym(count));
		}
		for (let p of promises) {
			let res = await p;
			timesAsym.push(...res)
		}

		promises = [];
		for (let i = 0; i < N; i++) {
			promises.push(philosophers[i].startConductor(count, waiter));
		}
		for (let p of promises) {
			let res = await p;
			timesWaiterForks.push(...res[0])
			timesWaiterWaiter.push(...res[1])
		}

		await writeToFile(asymFile, "a", N, timesAsym);
		await writeToFile(waiterForksFile, "a", N, timesWaiterForks);
		await writeToFile(waiterWaiterFile, "a", N, timesWaiterWaiter);
	}
}

// 0 - naive, 1 - asym, 2 - waiter
run(0);

// test(2, 100);

