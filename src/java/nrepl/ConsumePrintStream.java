package nrepl;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.function.Consumer;

/** Based on sun.rmi.runtime.Log.LoggerPrintStream */
public class ConsumePrintStream extends PrintStream  {

    private int last = -1;

    /** stream used for buffering lines */
    private final ByteArrayOutputStream bufOut;
    private final Consumer<String> consumer;

    public ConsumePrintStream(Consumer<String> consumer) {
        super(new ByteArrayOutputStream());
        bufOut = (ByteArrayOutputStream) super.out;
        this.consumer = consumer;
    }

    public void write(int b) {
        if ((last == '\r') && (b == '\n')) {
            last = -1;
            return;
        } else if ((b == '\n') || (b == '\r')) {
            try {
                /* Consume a single line */
                String line = bufOut.toString();
                consumer.accept(line);
            } finally {
                bufOut.reset();
            }
        } else {
            super.write(b);
        }
        last = b;
    }

    public void write(byte b[], int off, int len) {
        if (len < 0) {
            throw new ArrayIndexOutOfBoundsException(len);
        }
        for (int i = 0; i < len; i++) {
            write(b[off + i]);
        }
    }
}
