
export const syncPageToUrl_f =
pageStr => () => {
  const url = new URL(window.location);
  url.searchParams.set('page', pageStr);
  window.history.pushState(null, '', url.toString());
};

export const getPageFromUrl_f =
() => {
  const url = new URLSearchParams(window.location.search);
  return url.get('page') ?? '';
};
