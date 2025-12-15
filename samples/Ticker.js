
export const everyNSeconds =
n => eff => () => {
  const id = setInterval(eff, n * 1000);
  return () => clearInterval(id);
}

