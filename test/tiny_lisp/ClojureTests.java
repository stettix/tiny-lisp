package tiny_lisp;

import mikera.cljunit.NamespaceTest;

// See https://github.com/mikera/cljunit
public class ClojureTests extends NamespaceTest {
    @Override
    public String namespace() {
        return "tiny-lisp.core-test";
    }
}
