const myPromise = new Promise((resolve, reject) => {
  setTimeout(() => {console.log("spełniam obietnicę"); resolve('Gotowe!')}, 10);
});

setTimeout(() => {
  myPromise.then(val => console.log("odebralem ",val));
}, 1000);