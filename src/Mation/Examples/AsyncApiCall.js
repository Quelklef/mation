
export const afterNSeconds =
n => eff => () => {
  setTimeout(eff, n * 1000);
}

export const everyNSeconds =
n => eff => () => {
  const id = setInterval(eff, n * 1000);
  const cancel = () => clearInterval(id);
  return { cancel };
}

export const randNum =
() => {
  return Math.random();
};

export const strReverse =
string => {
  return Array.from(string).reverse().join('');
};
