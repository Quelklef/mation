
export const repeatedly =
eff => () => {
  eff();
  const id = setInterval(eff, 20);
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
