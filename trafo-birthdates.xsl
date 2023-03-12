<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs">
  <xsl:output method="xml" version="1.0" encoding="UTF-8" omit-xml-declaration="no" indent="yes" media-type="application/xml"/>
  <xsl:template match="text()"/>
  <xsl:key name="people-by-id" match="person" use="id"/>
  <xsl:key name="families-by-child" match="family" use="child"/>
  <xsl:key name="families-by-spouse" match="family" use="spouse"/>
  <xsl:key name="families-by-id" match="family" use="id"/>
  <xsl:template match="/xml[1]">
    <xsl:element name="mxfile">
      <xsl:attribute name="host">app.diagrams.net</xsl:attribute>
      <xsl:attribute name="modified"><!-- xsl:value-of select="current-dateTime()"/--></xsl:attribute>
      <xsl:attribute name="agent">5.0 (Windows)</xsl:attribute>
      <xsl:attribute name="etag"><xsl:value-of select="generate-id()"/></xsl:attribute>
      <xsl:attribute name="version">13.10.8</xsl:attribute>
      <xsl:attribute name="type">device</xsl:attribute>
      <xsl:apply-templates select="header[1]"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="header">
    <xsl:element name="diagram">
      <xsl:attribute name="id"><xsl:value-of select="generate-id()"/></xsl:attribute>
      <xsl:attribute name="name">Page-1</xsl:attribute>
      <xsl:element name="mxGraphModel">
        <xsl:attribute name="dx">0</xsl:attribute>
        <xsl:attribute name="dy">0</xsl:attribute>
        <xsl:attribute name="grid">1</xsl:attribute>
        <xsl:attribute name="gridSize">10</xsl:attribute>
        <xsl:attribute name="guides">1</xsl:attribute>
        <xsl:attribute name="tooltips">1</xsl:attribute>
        <xsl:attribute name="connect">1</xsl:attribute>
        <xsl:attribute name="arrows">1</xsl:attribute>
        <xsl:attribute name="fold">1</xsl:attribute>
        <xsl:attribute name="page">1</xsl:attribute>
        <xsl:attribute name="pageScale">1</xsl:attribute>
        <xsl:attribute name="pageWidth">850</xsl:attribute>
        <xsl:attribute name="pageHeight">1100</xsl:attribute>
        <xsl:attribute name="math">0</xsl:attribute>
        <xsl:attribute name="shadow">0</xsl:attribute>
        <xsl:element name="root">
          <xsl:element name="mxCell">
            <xsl:attribute name="id">0</xsl:attribute>
          </xsl:element>
          <xsl:element name="mxCell">
            <xsl:attribute name="id">1</xsl:attribute>
            <xsl:attribute name="parent">0</xsl:attribute>
          </xsl:element>
          <xsl:apply-templates select="/xml[1]/family[child]"/>
          <xsl:apply-templates select="/xml[1]/person[not(key('families-by-spouse',id)[1])]"/>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="child" mode="relate">
    <xsl:variable name="child_id" select="normalize-space(text())"/>
    <xsl:element name="mxCell">
      <xsl:attribute name="id"><xsl:text>relate-</xsl:text><xsl:value-of select="$child_id"/><xsl:text>-</xsl:text><xsl:value-of select="generate-id()"/></xsl:attribute>
      <xsl:attribute name="style">edgeStyle=orthogonalEdgeStyle;curved=1;rounded=1;orthogonalLoop=1;jettySize=auto;html=1;entryX=0.46;entryY=0.013;entryDx=0;entryDy=0;entryPerimeter=0;endArrow=none;endFill=0;</xsl:attribute>
      <!--xsl:attribute name="parent">1</xsl:attribute-->
      <xsl:attribute name="parent"><xsl:value-of select="../id/text()"/></xsl:attribute>
      <xsl:attribute name="source"><xsl:value-of select="../id/text()"/></xsl:attribute>
      <xsl:attribute name="target">
      <xsl:choose>
        <xsl:when test="key('families-by-spouse', $child_id)">
          <xsl:value-of select="key('families-by-spouse', $child_id)[1]/id/text()"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$child_id"/>
        </xsl:otherwise>
      </xsl:choose>
      </xsl:attribute>
      <xsl:attribute name="edge">1</xsl:attribute>
      <xsl:element name="mxGeometry">
        <xsl:attribute name="relative">1</xsl:attribute>
        <xsl:attribute name="as">geometry</xsl:attribute>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="spouse" mode="name">
    <xsl:copy-of select="translate(key('people-by-id', current()/text())[1]/name[1]/text(),'&quot;','')"/>
    <xsl:choose>
      <xsl:when test="position() = last()"/>
      <xsl:otherwise><xsl:text> &amp;amp; </xsl:text></xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="marriage" mode="attr">
    <xsl:if test="./date/text()">
      <xsl:text> &lt;br&gt;(</xsl:text>
      <xsl:value-of select="translate(./date/text(), '&quot;', '')"/>
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>
  <xsl:template match="family">
    <xsl:apply-templates select="child" mode="relate"/>
    <xsl:element name="mxCell">
      <xsl:attribute name="id"><xsl:value-of select='id/text()'/></xsl:attribute>
      <xsl:attribute name="value"><xsl:apply-templates select="spouse" mode="name"/><xsl:apply-templates select="marriage" mode="attr"/></xsl:attribute>
      <xsl:choose>
        <xsl:when test="endNode">
          <xsl:attribute name="style">rounded=1;whiteSpace=wrap;html=1;fillColor=#B5739D;strokeColor=#381410;fontColor=#ffffff;fontStyle=3;</xsl:attribute>
        </xsl:when>
        <xsl:when test="andThisNode">
          <xsl:attribute name="style">rounded=1;whiteSpace=wrap;html=1;fillColor=#CDE2BE;strokeColor=#781410;fontColor=#000000;fontStyle=1;</xsl:attribute>
        </xsl:when>
  <xsl:otherwise>
          <xsl:attribute name="style">rounded=1;whiteSpace=wrap;html=1;fillColor=#E6D0DE;strokeColor=#b85450;fontColor=#000000;</xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
        <xsl:when test="key('families-by-child',spouse[1]/text())">
          <xsl:attribute name="parent"><xsl:value-of select="key('families-by-child',spouse[1]/text())[1]/id/text()"/></xsl:attribute>
        </xsl:when>
        <xsl:when test="key('families-by-child',spouse[2]/text())">
          <xsl:attribute name="parent"><xsl:value-of select="key('families-by-child',spouse[2]/text())[1]/id/text()"/></xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
          <xsl:attribute name="parent">1</xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:attribute name="vertex">1</xsl:attribute>
      <xsl:element name="mxGeometry">
        <xsl:attribute name="width">320</xsl:attribute>
        <xsl:attribute name="height">60</xsl:attribute>
        <xsl:attribute name="x"><xsl:value-of select="10 + (24 * position())"/></xsl:attribute>
        <xsl:attribute name="y"><xsl:value-of select="16 + (56 * position())"/></xsl:attribute>
        <xsl:attribute name="as">geometry</xsl:attribute>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template name="live_dates">
    <xsl:if test="./birth/date/text() or ./death/date/text()">
      <xsl:text> &lt;br&gt;(</xsl:text>
      <xsl:value-of select="translate(./birth/date/text(), '&quot;', '')"/>
      <xsl:if test="./death/date/text()">
        <xsl:text> - </xsl:text>
        <xsl:value-of select="translate(./death/date/text(), '&quot;', '')"/>
      </xsl:if>
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>
  <xsl:template match="person">
    <xsl:element name="mxCell">
      <xsl:attribute name="id"><xsl:value-of select="id/text()"/></xsl:attribute>
      <xsl:attribute name="value"><xsl:value-of select="translate(name[1]/text(),'&quot;','')"/><xsl:call-template name="live_dates"/></xsl:attribute>
      <xsl:choose>
        <xsl:when test="endNode">
          <xsl:attribute name="style">shape=mxgraph.flowchart.terminator;whiteSpace=wrap;html=1;fillColor=#FFFF99;strokeColor=#020634;fontColor=#000000;fontStyle=3;strokeWidth=3;</xsl:attribute>
        </xsl:when>
        <xsl:when test="andThisNode">
          <xsl:attribute name="style">shape=mxgraph.flowchart.terminator;whiteSpace=wrap;html=1;fillColor=#99FFCC;strokeColor=#267342;fontColor=#000000;fontStyle=1;</xsl:attribute>
        </xsl:when>
        <xsl:when test="own-family and not(own-family/text() = '') and (key('families-by-id', current()/own-family/text())[*][child])">
          <xsl:attribute name="style">rounded=1;whiteSpace=wrap;html=1;fillColor=#fff2cc;strokeColor=#d6b656;fontColor=#000000;fontStyle=1;</xsl:attribute>
        </xsl:when>
  <xsl:otherwise>
          <xsl:attribute name="style">shape=mxgraph.flowchart.terminator;whiteSpace=wrap;html=1;fillColor=#99CCFF;strokeColor=#6682b3;fontColor=#000000;fontStyle=0;</xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
        <xsl:when test="key('families-by-child',current()/id/text())">
          <xsl:attribute name="parent"><xsl:value-of select="key('families-by-child',current()/id/text())[1]/id/text()"/></xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
          <xsl:attribute name="parent">1</xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:attribute name="vertex">1</xsl:attribute>
      <xsl:element name="mxGeometry">
        <xsl:attribute name="width">240</xsl:attribute>
        <xsl:attribute name="height">60</xsl:attribute>
        <xsl:attribute name="x"><xsl:value-of select="16 + (24 * position())"/></xsl:attribute>
        <xsl:attribute name="y"><xsl:value-of select="(64 * count(/xml[1]/family[child])) + (56 * position())"/></xsl:attribute>
        <xsl:attribute name="as">geometry</xsl:attribute>
      </xsl:element>
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>
