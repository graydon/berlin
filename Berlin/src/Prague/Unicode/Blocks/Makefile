# Makefile for unicode plugins
# Author:	Tobias Hunger

# General section
PLUGINS         = 0000-007F=Basic_Latin.so \
		  0080-00FF=Latin-1_Supplement.so \
		  0100-017F=Latin_Extended-A.so \
		  0180-024F=Latin_Extended-B.so \
		  0250-02AF=IPA_Extensions.so \
		  02B0-02FF=Spacing_Modifier_Letters.so \
		  0300-036F=Combining_Diacritical_Marks.so \
		  0370-03FF=Greek.so \
		  3400-4DB5=CJK_Unified_Ideographs_Extension_A.so \
		  4E00-9FFF=CJK_Unified_Ideographs.so \
		  AC00-D7A3=Hangul_Syllables.so \
		  D800-DB7F=High_Surrogates.so \
		  DB80-DBFF=High_Private_Use_Surrogates.so \
		  DC00-DFFF=Low_Surrogates.so \
		  E000-F8FF=Private_Use.so

PLUGINTARGET    = ../../../../modules/Unicode

INCLUDE         = -I../../../../include
LIBS            = -L../../../../lib -lPrague

# Program section
CC		= g++
RM		= rm -f

# Compiler section

#CDEBUGFLAGS	= -g

CCOPTIONS	= 
MKOBJFLAG	= -o # object code + output name
PROGNFLAG	= -o # specify output name (-o for gcc)
PLUGINFLAGS     = -shared -fpic

# Linker section
LDOPTIONS	=

# command section
LINK		= $(CC) $(PROGNFLAG) $(TARGET) 
# program section

PLUGINSRC       = $(PLUGINS:%.so=./%.cc)
PLUGINLIBS      = $(PLUGINS:%.so=$(PLUGINTARGET)/%.so)

# ----------------------------------------------------------------------------

all:		plugins

clean:		
		$(RM) $(PLUGINTARGET)/*.so

plugins:        $(PLUGINLIBS)

$(PLUGINTARGET)/%.so:   ./%.cc
		$(CC) $(INCLUDE) $(LIBS) $(PLUGINFLAGS) $(MKOBJFLAG) $@ $(CDEBUGFLAGS) $(CCOPTIONS) $<
