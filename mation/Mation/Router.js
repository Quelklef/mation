
export const readPathStr =
() => {
  const url = new URL(window.location.href);
  return url.pathname + url.search + url.hash;
};

export const writePathStr =
pathStr => () => {
  history.pushState(null, '', window.location.origin + pathStr);
};

export const encodeURIComponent = s => window.encodeURIComponent(s);
export const decodeURIComponent = s => window.decodeURIComponent(s);

