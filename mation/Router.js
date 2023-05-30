
export const readPathStr =
() => {
  return getPath(window.location.href);
};

function getPath(href) {
  const url = new URL(href);
  return url.pathname + url.search + url.hash;
}

export const writePathStr =
pathStr => () => {
  if (pathStr === readPathStr()) return;
  history.pushState({ isMation: true, path: pathStr }, '', window.location.origin + pathStr);
};

export const onPathStrChange =
listener => () => {
  window.addEventListener('popstate', ev => {
    if (!ev.state.isMation) return;  // Ignore out-of-framework route changes
    const pathStr = ev.state.path;
    listener(pathStr)();
  });
};

export const encodeURIComponent = s => window.encodeURIComponent(s);
export const decodeURIComponent = s => window.decodeURIComponent(s);

export const debounce =
ms => f => () => {
  let tid = 0;
  return x => () => {
    clearTimeout(tid);
    tid = setTimeout(() => f(x)(), ms);
  };
}

