
export const everyNSeconds =
n => eff => () => {
  eff();
  const id = setInterval(eff, n * 1000);
  const cancel = () => clearInterval(id);
  return { cancel };
};

export const parseInt =
s => {
  let n = +s;
  if (!Number.isFinite(n) && (n | 0) === n)
    n = 0;
  return n;
};

