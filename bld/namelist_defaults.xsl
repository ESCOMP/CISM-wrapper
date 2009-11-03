<?xml version='1.0'?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/">
  <html>
  <head>
    <title>Community Ice Sheet Model Namelist Defaults</title>
  </head>
  <body>
    <h2>Default values for namelist variables</h2>
    <p>Included in the table are the following pieces of information:</p>
    <ul>
       <li>Name of variable</li>
       <li>Horizontal grid resolution</li>
       <li>Land ocean mask type</li>
    </ul>

    <table BORDER="1" CELLPADDING="10">
      <th>Name</th>
      <th>Horz. Grid</th>
      <th>Mask</th>
      <th>Value</th>
      <xsl:for-each select="namelist_defaults/*">
      <xsl:sort select="name()"/>
      <tr>
        <td><font color="#ff0000">
        <xsl:value-of select="name()"/>
        </font></td>
        <td><xsl:value-of select="@hgrid"/></td>
        <td><xsl:value-of select="@mask"/></td>
        <td><xsl:value-of select="."    /></td>
      </tr>
      <tr>
      </tr>
      </xsl:for-each>
    </table>

  </body>

  </html>
</xsl:template>

<xsl:template match="name">
  <tr>
    <td><font color="#ff0000">----</font></td>
    <td><xsl:value-of select="@hgrid"/></td>
    <td><xsl:value-of select="@mask"/></td>
    <td><xsl:value-of select="@content"/></td>
    <td><xsl:apply-templates/></td>
  </tr>
</xsl:template>

<xsl:template match="name/varname">
  <font color="#ff0000"><xsl:apply-templates/></font>
</xsl:template>

<xsl:template match="name/default">
  <p><xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="name/listelm">
  <li><xsl:apply-templates/></li>
</xsl:template>

<xsl:template match="name/unlist">
  <ul>
  <xsl:apply-templates/>
  </ul>
</xsl:template>


</xsl:stylesheet>
