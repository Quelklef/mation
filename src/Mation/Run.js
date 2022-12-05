
export const getBody =
() => {
  return document.body;
};

export const mountUnder =
({ container, mountMe }) => () => {
  container.innerHTML = '';
  container.append(mountMe);
};

