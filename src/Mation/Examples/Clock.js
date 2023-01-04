
export const watchTime =
f => () => {
  function doIt() {
    const now = new Date();

    const second = now.getSeconds();
    const minute = now.getMinutes() + second / 60;
    const hour = (now.getHours() % 12) + minute / 60;

    f({ hour, minute, second })();
  }

  doIt();
  setInterval(doIt, 1000);
}

export const padStart =
n => p => s => {
  return s.padStart(n, p);
};
