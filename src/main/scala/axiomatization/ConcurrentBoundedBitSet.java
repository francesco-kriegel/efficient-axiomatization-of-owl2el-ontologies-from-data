package de.tu_dresden.inf.lat.axiomatization;


public class ConcurrentBoundedBitSet {

    private static final java.lang.invoke.VarHandle handle = java.lang.invoke.MethodHandles.arrayElementVarHandle(long[].class);
    private final long[] array;
    private final int maxElem;

    public ConcurrentBoundedBitSet(int maxElem) {
        this.array = new long[(maxElem >> 6) + 1];
        this.maxElem = maxElem;
    }

    public ConcurrentBoundedBitSet(long[] elems) {
        this.array = elems;
        this.maxElem = ((elems.length << 6) - 1);
    }

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

    public void add(int elem) {
        scala.Predef.require((0 <= elem) && (elem <= maxElem));
        handle.getAndBitwiseOr(array, elem >> 6, 1L << elem);
    }

    public void remove(int elem) {
        scala.Predef.require((0 <= elem) && (elem <= maxElem));
        handle.getAndBitwiseAnd(array, elem >> 6, ~(1L << elem));
    }

    public boolean contains(int elem) {
        return (0 <= elem) && (elem <= maxElem) && ((long) handle.getVolatile(array, elem >> 6) & (1L << elem)) != 0L;
    }

}
