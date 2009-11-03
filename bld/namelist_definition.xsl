<?xml version='1.0'?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/">
  <html>
    <xsl:apply-templates/>
  </html>
</xsl:template>

<xsl:template match="namelist_definition">
  <head>
    <title>Community Ice Sheet Model Namelist Definition</title>
  </head>
  <body>
    <h2>CISM namelist variables</h2>
    <p>Included in the table are the following pieces of information:</p>
    <ul>
    <li>Variable name.</li>
    <li>Variable type (<code>char</code>, <code>integer</code>,
    <code>real</code>, or <code>logical</code>).  The type
    <code>char</code> has the length appended
    following an asterisk, e.g., <code>char*256</code>.  Variables that are
    arrays have their dimension specifier appended inside parentheses.  For
    example <code>char*1(6)</code> denotes a array of six
    <code>char*1</code> values.
    </li>
    <li>Variable description (includes information on defaults).</li>
    <li>Valid values (if restricted).</li>
    </ul>

    <h3>CISM Case Information</h3>
    <table BORDER="1" CELLPADDING="10">
      <th>Name</th>
      <th>Type</th>
      <th>Description</th>
      <th>Valid values</th>
      <xsl:apply-templates select="entry[@category='case']"/>
    </table>

    <h3>CISM File Options</h3>
    <table BORDER="1" CELLPADDING="10">
      <th>Name</th>
      <th>Type</th>
      <th>Description</th>
      <th>Valid values</th>
      <xsl:apply-templates select="entry[@category='files']"/>
    </table>

    <h3>CISM Time Management</h3>
    <table BORDER="1" CELLPADDING="10">
      <th>Name</th>
      <th>Type</th>
      <th>Description</th>
      <th>Valid values</th>
      <xsl:apply-templates select="entry[@category='time']"/>
    </table>

    <h3>CISM Starting Time Specification</h3>
    <table BORDER="1" CELLPADDING="10">
      <th>Name</th>
      <th>Type</th>
      <th>Description</th>
      <th>Valid values</th>
      <xsl:apply-templates select="entry[@category='start-time']"/>
    </table>


  </body>
</xsl:template>

<xsl:template match="entry">
  <tr>
    <td><font color="#ff0000"><xsl:value-of select="@id"/></font></td>
    <td><xsl:value-of select="@type"/></td>
    <td><xsl:apply-templates/></td>
    <td><xsl:value-of select="@valid_values"/></td>
  </tr>
</xsl:template>

<xsl:template match="entry/varname">
  <font color="#ff0000"><xsl:apply-templates/></font>
</xsl:template>

<xsl:template match="entry/default">
  <p><xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="entry/listelm">
  <li><xsl:apply-templates/></li>
</xsl:template>

<xsl:template match="entry/unlist">
  <ul>
  <xsl:apply-templates/>
  </ul>
</xsl:template>


</xsl:stylesheet>
