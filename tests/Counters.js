
export const everyNSeconds =
n => eff => () => {
  eff();
  const id = setInterval(eff, n * 1000);
  const cancel = () => clearInterval(id);
  return { cancel };
};
