function printAsync(s) {
    var delay = Math.floor((Math.random()*1000)+500);
    return new Promise((resolve,reject)=> {
        setTimeout(function() {
            console.log(s);
            resolve();
    }, delay)});
 }


// 'then' returns a new Promise, therefore we can chain another 'then'.
// In this case 'printAsync(s)' directly returns a Promise object, however
// 'then' could also return a value in which case it would be wrapped
// in a Promise that would be automatically resolved with that value.
 printAsync(1)
  .then(() => printAsync(2))
  .then(() => printAsync(3))
  .catch((error) => {
    console.error("An error occurred:", error);
  });

 