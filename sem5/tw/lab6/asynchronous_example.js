
const crypto = require('crypto');

// większa liczba iteracji = dłuższy czas generacji skrotu 
const crypto_iterations=1000000
const timeoutScheduled1 = Date.now();
// wywołanie asynchroniczne zlecane puli wątków 
//https://nodejs.org/en/learn/asynchronous-work/dont-block-the-event-loop#what-code-runs-on-the-worker-pool
crypto.pbkdf2('mysecret1', 'salt', crypto_iterations, 64,'sha512', (err, derivedKey) => {
    if (err) throw err;
    console.log(derivedKey.toString('hex'));
    const delay = Date.now() - timeoutScheduled1;
    console.log(`skonczylem generowac skrot w ${delay} ms`);}
);

console.log("Robie cos !")

const timeoutScheduled2 = Date.now();
setTimeout(() => {
  const delay = Date.now() - timeoutScheduled2;
  console.log(`test setTimeout czas ${delay} ms`);
}, 200);

console.log("Robie cos !")


