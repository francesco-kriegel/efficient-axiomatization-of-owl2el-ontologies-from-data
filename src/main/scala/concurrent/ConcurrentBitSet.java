package de.tu_dresden.inf.lat.concurrent;

public class ConcurrentBitSet {

//    public static final java.lang.invoke.VarHandle varHandleArray;
//
//    static {
//        try {
//            varHandleArray = java.lang.invoke.MethodHandles.lookup().findVarHandle(ConcurrentBitSetJava.class, "array", long[].class);
//        } catch (ReflectiveOperationException e) {
//            throw new ExceptionInInitializerError(e);
//        }
//    }

    private static final java.lang.invoke.VarHandle handle = java.lang.invoke.MethodHandles.arrayElementVarHandle(long[].class);

    private final java.util.concurrent.ConcurrentLinkedQueue<long[]> previousArrays = new java.util.concurrent.ConcurrentLinkedQueue<>();
    private volatile long[] array = new long[1];
    private volatile boolean allowResize = true;

    public ConcurrentBitSet() {}

    public scala.collection.immutable.BitSet viewAsImmutableBitSet() {
        return scala.collection.immutable.BitSet$.MODULE$.fromBitMaskNoCopy(array);
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

    public void add(int elem) {
        scala.Predef.require(0 <= elem);
        final int idx = elem >> 6;
        synchronized (previousArrays) {
            if (idx >= array.length) {
                if (allowResize) {
                    previousArrays.add(array);
                    array = java.util.Arrays.copyOf(array, idx + 1);
                } else {
                    throw new RuntimeException();
                }
            }
        }
        handle.getAndBitwiseOr(array, idx, 1L << elem);
    }

    public void remove(int elem) {
        if (allowResize) {
            throw new RuntimeException();
        } else {
            scala.Predef.require((0 <= elem) && (elem <= maximalPossibleElement()));
            handle.getAndBitwiseAnd(array, elem >> 6, ~(1L << elem));
        }
    }

    public boolean contains(int elem) {
        if (allowResize) {
            throw new RuntimeException();
        } else {
            return (0 <= elem) && (elem <= maximalPossibleElement())
                    && ((long) handle.getVolatile(array, elem >> 6) & (1L << elem)) != 0L;
        }
    }

}
