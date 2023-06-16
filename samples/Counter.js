
export const repeatedly =
({ nTimes, delaySeconds }) => fn => () => {
  if (!nTimes) return;
  fn();
  let n = nTimes - 1;
  const id = setInterval(() => {
    if (n) {
      fn();
      n--;
    } else {
      clearInterval(id);
    }
  }, delaySeconds * 1000);
};
