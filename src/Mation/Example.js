
export const repeatedly =
eff => () => {
  eff();
  const id = setInterval(eff, 20);
  const cancel = () => clearInterval(id);
  return { cancel };
};

export const getValue =
ev => {
  return ev.target.value;
};
