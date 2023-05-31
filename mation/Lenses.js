
export const unsafeTake =
keys => record => {
  const result = {};
  for (const key of keys)
    result[key] = record[key];
  return result;
};
