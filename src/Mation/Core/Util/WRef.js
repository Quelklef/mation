
/*

We represent a reference as a { get, set, doOnNextChange } triplet

This somewhat-strange representation allows for an ergonomic and
performant implementation of WRef views

*/

export const new_f =
f => () => {

  let value = null;
  let waiting = () => {};  // next change listener

  const ref = {
    get() {
      return value;
    },
    set(newVal) {
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

  ref.set(f(ref));

  return ref;

};

export const get =
ref => () => {
  return ref.get();
};

export const set =
newVal => ref => () => {
  ref.set(newVal);
};

export const nextChange =
f => ref => () => {
  ref.doOnNextChange(f);
};

export const mkView_f =
({ getter, setter }) => largeRef => {
  return {
    get() {
      const large = largeRef.get();
      const small = getter(large);
      return small;
    },
    set(newSmall) {
      const large = largeRef.get();
      const large_ = setter(newSmall)(large);
      largeRef.set(large_);
    },
    doOnNextChange(f) {
      largeRef.doOnNextChange(f);
      // ^ May invoke false-positives, but also avoids an Eq constraint
    },
  };
};
