Copyright @lucabotez

# Suffix Trees

## Overview
This project implements a **suffix tree** in **Racket**, a powerful data structure used for **fast substring searches, pattern matching, and text processing**. The implementation is structured in stages, progressively building the suffix tree and handling stream-based suffix trees for efficient memory usage.

## Features
- **Efficient substring search** using a suffix tree.
- **Incremental construction** with multiple stages.
- **Support for large-scale text processing** via stream-based suffix trees.
- **Optimized data representation** using functional programming paradigms in Racket.

## Project Structure
- **`collection.rkt`** – Defines core utility functions used across the project.
- **`stage1.rkt`** – Implements the basic suffix tree structure.
- **`stage2.rkt`** – Expands functionality with substring searching.
- **`stage3.rkt`** – Optimizes memory usage and tree traversal.
- **`stage4.rkt`** – Implements additional suffix tree operations.
- **`suffix-tree.rkt`** – Main file integrating all stages into a complete suffix tree implementation.
- **`suffix-tree-stream.rkt`** – Implements a streaming version of the suffix tree for handling large datasets.

## Notes
- The **suffix tree** significantly reduces substring search complexity from **O(n*m)** to **O(n)**.
- The **stream-based implementation** allows efficient handling of large inputs without excessive memory consumption.
