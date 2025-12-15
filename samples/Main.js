
export const setUrlHash = s => () => {
  document.location.hash = s;
};

export const getUrlHash = () => {
  return (
    document.location.hash
    .slice(1)  // drop leading '#'
  );
};
