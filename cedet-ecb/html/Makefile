# This file generates the HTML-website of ECB

# Copyright (C) 2000 - 2005 Jesper Nordenberg,
#                           Klaus Berndl,
#                           Free Software Foundation, Inc.

# Author: Jesper Nordenberg <mayhem@home.se>
#         Klaus Berndl <klaus.berndl@sdm.de>
# Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
# Keywords: browser, code, programming, tools
# Created: 2004

# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# GNU Emacs; see the file COPYING.  If not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# $Id$


# Just type "make upload" to upload a complete new build of the whole
# website to SourgeForge.
#
# Or type "make all" or "make upload-build" to generate the whole website
# and build an uploadable ecb-website.tar.gz file.

# $Id$

EMACS=emacs

include ../ecb-makedef.mk

TARC=tar -czvf
SCP=scp
ecb_SCP_login=berndl,ecb@web.sourceforge.net
#SSH=ssh -l berndl ecb.sourceforge.net
SSH=ssh -t berndl,ecb@shell.sourceforge.net create

ecb_ECB_LOC=/home/groups/e/ec/ecb
ecb_WEBSITE_LOC=htdocs

GNUCLIENT=gnuclient

CEDETLOAD=c:/Programme/emacs-22.3/site-lisp/package-development/cedet/common/cedet.el

MAKEINFO=/usr/bin/makeinfo

ecb_HTMLSOURCE=ecb-html.el
ecb_HTML_GEN=logo.html javabrowser.html main.html menu.html links.html \
             top.html all-news.html downloads.html cvs.html
ecb_HTMLDOCU_DIR=./docs
ecb_TEXI_FILE=../ecb.texi

ecb_SCREENSHOTS=screenshots/index.html screenshots/*.png

ecb_GIFS=bullet.gif ecb_logo.gif mail.gif smiley.gif

ecb_UPLOAD_ARC_UNZIPPED=ecb-website.tar
ecb_UPLOAD_ARC=$(ecb_UPLOAD_ARC_UNZIPPED).gz

ecb_WEBSITE_CONTENT=index.html $(ecb_HTML_GEN) $(ecb_HTMLDOCU_DIR)/*.html \
                    $(ecb_SCREENSHOTS) NEWS.html $(ecb_GIFS)


help:
	@echo "The following external targets are available:"
	@echo "- website: Builds all html-files for the ECB-homepage."
	@echo "- clean: Deletes all generated files."
	@echo "- upload-build: Generate a zipped tar-archive for a complete"
	@echo "                update of the ECB-homepage. Calls all necessary"
	@echo "                targets."
	@echo "- upload: Uploads a complete new built ECB-homepage. Calls "
	@echo "          upload-build."
	@echo "- upload-update: Uploads all files which reside in the subdir"
	@echo "                 'files-for-upload' (incl. the patches in the"
	@echo "                 subdir files-for-upload/patches'."
	@echo "- update-cvs-snapshot: Updates the latest stable CVS-snapshot"
	@echo "                       at the ECB-website"
	@echo "All 'upload*'-targets needs a running Emacs-instance connectable"
	@echo "by gnuclient."

all: upload-build

# Creates the html-files from ecb-html.el
website: $(ecb_HTMLSOURCE)
	@$(RM) $(ecb_HTML_GEN)
	@$(RM) *.elc
	$(EBATCH) -l $(CEDETLOAD) $< -f eval-current-buffer

# Generates HTML-docu from ecb.texi into ./docs/
online-docu: $(ecb_TEXI_FILE)
	@$(MKDIR) $(ecb_HTMLDOCU_DIR)
	@$(RM) $(ecb_HTMLDOCU_DIR)/*.html
	$(MAKEINFO) --html --output=$(ecb_HTMLDOCU_DIR) $<

$(ecb_HTMLDOCU_DIR)/index.html: online-docu

NEWS.html:
	@$(CP) ../NEWS .
	@$(RM) NEWS.html
	@$(GNUCLIENT) -e '(progn (find-file "NEWS") (save-excursion (end-of-buffer) (previous-line 6) (delete-region (point) (point-max))) (load-library "htmlfontify") (htmlfontify-buffer) (write-file "NEWS.html") (kill-buffer "NEWS.html") (switch-to-buffer "NEWS") (set-buffer-modified-p nil) (kill-buffer "NEWS"))'
	@$(RM) NEWS


# Creates a complete new build of the whole website
upload-build: $(ecb_HTMLDOCU_DIR)/index.html NEWS.html website
	@$(RM) $(ecb_UPLOAD_ARC)
	$(TARC) $(ecb_UPLOAD_ARC) $(ecb_WEBSITE_CONTENT)

# Uploads a complete new website build to ecb.sourceforge.net
upload: website upload-build
	$(SCP) $(ecb_UPLOAD_ARC) $(ecb_SCP_login):$(ecb_WEBSITE_LOC)
	@echo "For unpacking and installing the new contents do:"
	@echo "1. open a shell at SF with ssh: ssh -t berndl,ecb@shell.sourceforge.net create"
	@echo "2. Run ./update_website.sh (this extracts the archive and performs all chmods)"

# Uploads all files currently in ./files-for-upload
upload-update:
	@(cd files-for-upload; \
	$(TARC) $(ecb_UPLOAD_ARC) *; \
	echo "Uploading the updates..."; \
	$(SCP) $(ecb_UPLOAD_ARC) $(ecb_SCP_login):$(ecb_WEBSITE_LOC); \
	echo "1. open a shell at SF with ssh: ssh -t berndl,ecb@shell.sourceforge.net create"; \
	echo "2. Run ./update_website.sh (this extracts the archive and performs all chmods)"; \
	$(RM) $(ecb_UPLOAD_ARC))


update-cvs-snapshot:
	@echo "Updating latest stable CVS-snapshot..."
	@$(RM) ecb.tar.gz
	@$(RM) -R ecb-snap
	@$(MKDIR) ecb-snap
	@(cd ..; \
	$(CP) $(ecb_DISTRIB_FILES) html/ecb-snap; \
	$(CP) -r $(ecb_IMAGE_DIR) html/ecb-snap)
	@find ecb-snap/$(ecb_IMAGE_DIR) -name CVS -print | xargs rm -Rf
	@find ecb-snap/$(ecb_IMAGE_DIR) -name *~ -print | xargs $(RM)
	@find ecb-snap/$(ecb_IMAGE_DIR) -name *.png -print | xargs $(RM)
	@$(TARC) ecb.tar.gz ecb-snap
	@$(RM) -R ecb-snap
	$(SCP) ecb.tar.gz $(ecb_SCP_login):$(ecb_WEBSITE_LOC)/cvs_snapshots
	@$(RM) ecb.tar.gz


clean:
	@$(RM) -R $(ecb_HTMLDOCU_DIR)
	@$(RM) NEWS.html *.elc $(ecb_UPLOAD_ARC) $(ecb_HTML_GEN) *.*~
	@(cd files-for-upload/patches; \
	   $(RM) *.*)
	@(cd files-for-upload; \
	   $(RM) *.*)


