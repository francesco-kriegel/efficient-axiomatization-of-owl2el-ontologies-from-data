package de.tu_dresden.inf.lat.axiomatization;

import org.semanticweb.owlapi.model.*;
import uk.ac.manchester.cs.owl.owlapi.InitVisitorFactory;
import uk.ac.manchester.cs.owl.owlapi.Internals;
import uk.ac.manchester.cs.owl.owlapi.MapPointer;
import uk.ac.manchester.cs.owl.owlapi.OWLAxiomIndexImpl;
import uk.ac.manchester.cs.owl.owlapi.concurrent.ConcurrentOWLOntologyImpl;

import javax.annotation.ParametersAreNonnullByDefault;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.stream.Stream;

import static org.semanticweb.owlapi.model.AxiomType.OBJECT_PROPERTY_ASSERTION;

/**
 * A class that encapsulates an instance of {@link OWLOntology} and adds a further index
 * mapping each instance of {@link OWLIndividual} to the set of all instances of
 * {@link OWLObjectPropertyAssertionAxiom} in which it occurs in object position.
 */
public class OWLOntologyWithFurtherIndexes {

    /* Creating a new visitor that is not defined in uk.ac.manchester.cs.owl.owlapi.InitVisitorFactory */
    private static final InitVisitorFactory.InitVisitor<OWLIndividual> INDIVIDUALSUPERNAMED =
            new ConfigurableInitIndividualVisitor<>(false, true);

    private final OWLOntology ontology;
    private transient MapPointer<OWLIndividual, OWLObjectPropertyAssertionAxiom> objectPropertyAssertionsByObjectIndividual;

    private final OWLAxiomVisitor addObjectPropertyAssertionVisitor =
            new OWLAxiomVisitor() {
                @Override
                public void visit(OWLObjectPropertyAssertionAxiom axiom) {
                    objectPropertyAssertionsByObjectIndividual.put(axiom.getObject(), axiom);
                }
            };

    private final OWLAxiomVisitor removeObjectPropertyAssertionVisitor =
            new OWLAxiomVisitor() {
                @Override
                public void visit(OWLObjectPropertyAssertionAxiom axiom) {
                    objectPropertyAssertionsByObjectIndividual.remove(axiom.getObject(), axiom);
                }
            };

    public OWLOntologyWithFurtherIndexes(OWLOntology ontology) {
        super();
        this.ontology = ontology;
        initializeLazyObjectPropertyAssertionsMap();
        ontology.getOWLOntologyManager()
                .addOntologyChangeListener(
                        changes ->
                                changes.stream()
                                        .filter(change -> change.isAxiomChange(OBJECT_PROPERTY_ASSERTION))
                                        .forEach(change -> {
                                            if (change.isAddAxiom()) {
                                                change.getAddedAxiom().get().accept(addObjectPropertyAssertionVisitor);
                                            } else if (change.isRemoveAxiom()) {
                                                change.getRemovedAxiom().get().accept(removeObjectPropertyAssertionVisitor);
                                            }
                                        }),
                        new SpecificOntologyChangeBroadcastStrategy(ontology)
                );
    }

    @SuppressWarnings("unchecked")
    private void initializeLazyObjectPropertyAssertionsMap() {
        try {
            final Field intsField = OWLAxiomIndexImpl.class.getDeclaredField("ints");
            intsField.setAccessible(true);
            final Internals ints;
            if (ontology instanceof ConcurrentOWLOntologyImpl) {
                final Field delegateField = ConcurrentOWLOntologyImpl.class.getDeclaredField("delegate");
                delegateField.setAccessible(true);
                ints = (Internals) intsField.get((OWLOntology) delegateField.get(ontology));
            } else {
                ints = (Internals) intsField.get(ontology);
            }
            final Method buildLazyMethod =
                    Arrays.stream(Internals.class.getDeclaredMethods())
                            .filter(method -> method.getName().equals("buildLazy"))
                            .findAny().get();
            buildLazyMethod.setAccessible(true);
            objectPropertyAssertionsByObjectIndividual =
                    (MapPointer<OWLIndividual, OWLObjectPropertyAssertionAxiom>)
                            buildLazyMethod.invoke(ints, OBJECT_PROPERTY_ASSERTION, INDIVIDUALSUPERNAMED, OWLObjectPropertyAssertionAxiom.class);
        } catch (NoSuchFieldException | IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    public final OWLOntology getOntology() {
        return ontology;
    }

    public final Stream<OWLObjectPropertyAssertionAxiom> objectPropertyAssertionAxiomsWithSubject(OWLIndividual individual) {
        /* The OWLAPI already manages an index for the desired results, so there is no need to create a second one. */
        return ontology.objectPropertyAssertionAxioms(individual);
    }

    public final Stream<OWLObjectPropertyAssertionAxiom> objectPropertyAssertionAxiomsWithObject(OWLIndividual individual) {
        return objectPropertyAssertionsByObjectIndividual.getValues(individual);
    }

//    public ChangeApplied addAxiom(OWLAxiom axiom) {
//        final ChangeApplied result = ontology.addAxiom(axiom);
//        if (result.equals(ChangeApplied.SUCCESSFULLY) && axiom.getAxiomType().equals(OBJECT_PROPERTY_ASSERTION)) {
//            axiom.accept(addObjectPropertyAssertionVisitor);
//        }
//        return result;
//    }
//
//    public ChangeApplied removeAxiom(OWLAxiom axiom) {
//        final ChangeApplied result = ontology.removeAxiom(axiom);
//        if (result.equals(ChangeApplied.SUCCESSFULLY) && axiom.getAxiomType().equals(OBJECT_PROPERTY_ASSERTION)) {
//            axiom.accept(removeObjectPropertyAssertionVisitor);
//        }
//        return result;
//    }

    private static class ConfigurableInitIndividualVisitor<K extends OWLObject> extends InitVisitorFactory.InitIndividualVisitor<K> {

        private final boolean sub;

        public ConfigurableInitIndividualVisitor(boolean sub, boolean named) {
            super(sub, named);
            this.sub = sub;
        }

        @Override
        @SuppressWarnings("unchecked")
        @ParametersAreNonnullByDefault
        public K visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
            if (sub) {
                return (K) axiom.getSubject();
            } else {
                return (K) axiom.getObject();
            }
        }

        @Override
        @SuppressWarnings("unchecked")
        @ParametersAreNonnullByDefault
        public K visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
            if (sub) {
                return (K) axiom.getSubject();
            } else {
                return (K) axiom.getObject();
            }
        }

        @Override
        @SuppressWarnings("unchecked")
        @ParametersAreNonnullByDefault
        public K visit(OWLObjectPropertyAssertionAxiom axiom) {
            if (sub) {
                return (K) axiom.getSubject();
            } else {
                return (K) axiom.getObject();
            }
        }

        @Override
        @SuppressWarnings("unchecked")
        @ParametersAreNonnullByDefault
        public K visit(OWLDataPropertyAssertionAxiom axiom) {
            if (sub) {
                return (K) axiom.getSubject();
            } else {
                return (K) axiom.getObject();
            }
        }

    }

}
