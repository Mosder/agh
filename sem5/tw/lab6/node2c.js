const myPromise = Promise.resolve('wynik pierwszego');
console.log("robie cos1")


setTimeout(() => {
  myPromise.then(val => console.log("wynik drugiego odebrał", val));
}, 1000);
console.log("robie cos2")