
export const watchTime =
f => () => {
  function doIt() {
    const now = new Date();

    const hour = now.getHours() % 12;
    const minute = now.getMinutes();
    const second = now.getSeconds();

    f({ hour, minute, second })();
  }

  doIt();
  setInterval(doIt, 1000);
}

export const padStart =
n => p => s => {
  return s.padStart(n, p);
};
