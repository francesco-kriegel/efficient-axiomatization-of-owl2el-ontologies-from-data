package de.tu_dresden.inf.lat.axiomatization;


public class ConcurrentBitSet {

    private static final java.lang.invoke.VarHandle handle = java.lang.invoke.MethodHandles.arrayElementVarHandle(long[].class);
    private final java.util.concurrent.ConcurrentLinkedQueue<long[]> previousArrays = new java.util.concurrent.ConcurrentLinkedQueue<>();
    private volatile long[] array = new long[1];
    private volatile boolean allowResize = true;

    public ConcurrentBitSet() {}

    public scala.collection.immutable.BitSet viewAsImmutableBitSet() {
        return scala.collection.immutable.BitSet$.MODULE$.fromBitMaskNoCopy(array);
    }

    public scala.collection.mutable.BitSet viewAsMutableBitSet() {
        return scala.collection.mutable.BitSet$.MODULE$.fromBitMaskNoCopy(array);
    }

    public scala.collection.immutable.BitSet copyToImmutableBitSet() {
        return scala.collection.immutable.BitSet$.MODULE$.fromBitMask(array);
    }

    public scala.collection.mutable.BitSet copyToMutableBitSet() {
        return scala.collection.mutable.BitSet$.MODULE$.fromBitMask(array);
    }

    public void allowResize() {
        synchronized (previousArrays) {
            allowResize = true;
        }
    }

    public void disallowResize() {
        synchronized (previousArrays) {
            if (allowResize) {
                allowResize = false;
                consolidateArray();
            }
        }
    }

    private void consolidateArray() {
        while (!previousArrays.isEmpty()) {
            long[] previousArray = previousArrays.poll();
            if (previousArray.length > array.length) {
                throw new RuntimeException();
            }
            for (int idx = 0; idx < previousArray.length; idx++) {
                handle.getAndBitwiseOr(array, idx, previousArray[idx]);
            }
        }
    }

    public int maximalPossibleElement() {
        if (allowResize) {
            return Integer.MAX_VALUE;
        } else {
            return (array.length << 6) - 1;
        }
    }

    public int minimalPossibleElement() {
        return 0;
    }

    public void add(int elem) throws IllegalStateException {
        scala.Predef.require(0 <= elem);
        final int idx = elem >> 6;
        synchronized (previousArrays) {
            if (idx >= array.length) {
                if (allowResize) {
                    previousArrays.add(array);
                    array = java.util.Arrays.copyOf(array, idx + 1);
                } else {
                    throw new IllegalStateException("The element cannot be added since, currently, it is larger than supported and resizing is disallowed.");
                }
            }
        }
        handle.getAndBitwiseOr(array, idx, 1L << elem);
    }

    public void remove(int elem) throws IllegalStateException {
        if (allowResize) {
            throw new IllegalStateException("The method `remove` may be called only if resizing is disallowed.");
        } else {
            scala.Predef.require((0 <= elem) && (elem <= maximalPossibleElement()));
            handle.getAndBitwiseAnd(array, elem >> 6, ~(1L << elem));
        }
    }

    public boolean contains(int elem) throws IllegalStateException {
        if (allowResize) {
            throw new IllegalStateException("The method `contains` may be called only if resizing is disallowed.");
        } else {
            return (0 <= elem) && (elem <= maximalPossibleElement())
                    && ((long) handle.getVolatile(array, elem >> 6) & (1L << elem)) != 0L;
        }
    }

}
