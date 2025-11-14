# ==============================================
# Stage 1 — Build Lisp binary
# ==============================================
FROM debian:bookworm-slim AS builder

LABEL stage="builder" maintainer="you@example.com"

# Install SBCL and dependencies
RUN apt-get update && \
    apt-get install -y sbcl curl git && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

# Ensure Quicklisp is installed if missing
RUN if [ ! -d "/app/quicklisp" ]; then \
    echo "Installing Quicklisp..."; \
    curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:add-to-init-file)' \
    --quit; \
    fi

# Build the Lisp executable
RUN sbcl --non-interactive \
    --load "/app/run.lisp" \
    --eval "(save-lisp-and-die \"app\" :executable t :toplevel #'my-rest-server::main)"

# ==============================================
# Stage 2 — Runtime image
# ==============================================
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y libssl3 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/app /app/app

EXPOSE 7000
CMD ["/app/app"]
