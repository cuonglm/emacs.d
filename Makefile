help:
	@echo "  deps        Install dependencies."

deps:
	easy_install pip && \
	pip install -r python_requirements.txt && \
	apt-get install clang shellcheck
