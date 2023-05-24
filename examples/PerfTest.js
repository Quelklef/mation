
export const toFixed =
k => n => {
  if (k === 0)
    return '' + Math.round(n);
  return n.toFixed(k);
};

export const timeMe =
block => () => {
  const t0 = Date.now();
  block();
  const dt = Date.now() - t0;
  return dt;
};

export const getNow =
() => {
  return +new Date;
};

export const calcStats =
nums => {

  let sum = 0;
  let cnt = 0;

  for (const num of nums) {
    sum += num;
    cnt += 1;
  }

  const mean = sum / cnt;

  let sumsq = 0;
  for (const num of nums) {
    sumsq += Math.pow(num - mean, 2);
  }

  const stdev = Math.sqrt(sumsq / cnt);

  const sorted = [...nums].sort((a, b) => a - b);
  const median = sorted[Math.round(sorted.length / 2)] ?? Number.NaN;

  return { mean, median, stdev };

};

export const foldMapWithIndex_f =
append => mempty =>
f => arr => {
  let r = mempty;
  for (let i = 0; i < arr.length; i++) {
    r = append(r)(f(i)(arr[i]));
  }
  return r;
};

