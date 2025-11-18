let externalResolve;

const pendingPromise = new Promise((resolve, reject) => {
  console.log("Pending Promise - zapamiętuje resolve do późniejszego użycia ")
  externalResolve  = resolve;
});

console.log("robie cos 1")
pendingPromise.then(result => {
  console.log("Promise zakończony:", result);
});

console.log("robie cos 2")
// tutaj dopiero robię resolve na pierwszym Promise !
externalResolve("moj wynik");
console.log("robie cos 3")

