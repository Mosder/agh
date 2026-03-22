#!/bin/bash
cd $(dirname ${BASH_SOURCE[0]})
.venv/bin/uvicorn server:app --reload
