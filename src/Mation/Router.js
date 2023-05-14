
export const readPath_f =
toMaybe => () => {

  const url = new URL(window.location.href);

  const parts = (
    url.pathname
    .split('/')
    .filter(part => !!part)  // remove trailing '/' if there is one
    .map(part => decodeURIComponent(part))
  );

  const params = (
    url.search
    .slice(1)  // remove leading '?'
    .split('&')
    .map(pair => {
      let [k, v] = pair.split('=');
      k = decodeURIComponent(k);
      v = pair.includes('=') ? decodeURIComponent(v) : null;
      return [k, toMaybe(v)];
    })
  );

  const fragment = (
    url.hash
    ? (
      url.hash
      .slice(1)  // remove leading '#'
    )
    : null
  );

  return { parts, params, fragment: toMaybe(fragment) };

};


export const writePath_f =
toNullable => path => () => {

  const url = new URL(window.location.href);

  url.pathname = (
    path.parts
    .map(part => encodeURIComponent(part))
    .join('/')
  );

  url.search = (
    path.params.length === 0 ? ''
    : (
        '?' +
        path.params
        .map(([k, v]) => {
          v = toNullable(v);
          if (v !== null) {
            return encodeURIComponent(k) + '=' + encodeURIComponent(v);
          } else {
            return encodeURIComponent(k);
          }
        })
        .join('&')
    )
  );

  const frag = toNullable(path.fragment);
  url.hash = (
    frag
      ? '#' + encodeURIComponent(frag)
      : ''
  );

  history.pushState(null, '', url.toString());

};

