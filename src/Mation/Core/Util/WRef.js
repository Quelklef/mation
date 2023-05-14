
/*

We represent a reference as a { get, set, doOnNextChange } triplet

This somewhat-strange representation allows for an ergonomic and
performant implementation of WRef views

*/

export const make_f =
f => () => {

  let value = null;
  let waiting = () => {};  // next change listener

  const ref = {
    read() {
      return value;
    },
    write(newVal) {
      value = newVal;
      const waiting0 = waiting;
      waiting = () => {};
      waiting0();
    },
    doOnNextChange(f) {
      const waiting0 = waiting;
      waiting = () => { waiting0(); f(); };
    },
  };

  ref.write(f(ref));

  return ref;

};

export const read =
ref => () => {
  return ref.read();
};

export const write =
newVal => ref => () => {
  ref.write(newVal);
};

export const nextChange =
f => ref => () => {
  ref.doOnNextChange(f);
};

export const mkView_f =
({ getter, setter }) => largeRef => {
  return {
    read() {
      const large = largeRef.read();
      const small = getter(large);
      return small;
    },
    write(newSmall) {
      const large = largeRef.read();
      const large_ = setter(newSmall)(large);
      largeRef.write(large_);
    },
    doOnNextChange(f) {
      largeRef.doOnNextChange(f);
      // ^ May invoke false-positives, but also avoids an Eq constraint
    },
  };
};
