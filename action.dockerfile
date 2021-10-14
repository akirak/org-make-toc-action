FROM ghcr.io/akirak/org-make-toc:latest
ADD entrypoint.sh /bin/entrypoint.sh
ENTRYPOINT ["/bin/bash", "/bin/entrypoint.sh"]
