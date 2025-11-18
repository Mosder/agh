function printAsync(s) {
    var delay = Math.floor((Math.random()*1000)+500);
    return new Promise((resolve,reject)=> {
        setTimeout(function() {
            console.log(s);
            resolve();
    }, delay)});
 }

function print123() {
	return new Promise((resolve, reject) => {
		printAsync(1)
		.then(() => printAsync(2))
		.then(() => printAsync(3))
		.then(() => resolve());
	});
}

function loopr(n) {
	if (n <= 0) return;
	print123().then(() => loopr(n-1));
}

function loopi(n) {
	let task = new Promise((res, rej) => res());
	for (let i = 0; i < n; i++) {
		task = task.then(() => print123());
	}
}

  /* 
** Cwiczenie:
** Napisz dwie funkcje loopr(n) i loopi(n) ktore powodują wykonanie powyzszej 
** sekwencji zadan n razy. Czyli: 1 2 3 1 2 3 1 2 3 ... done
** loopr - rekurencyjnie loopi - iteracyjnie.
*/

// loopr(4);
loopi(4);
