interface Props {
  counts: [string, number][]; // [company, count], pre-sorted descending
}

export function CompanyBarChart({ counts }: Props) {
  if (counts.length === 0) return null;
  const max = counts[0][1];

  return (
    <div className="panel">
      <h2>Open roles by company</h2>
      {counts.map(([company, count]) => (
        <div className="bar-row" key={company}>
          <div className="bar-label" title={company}>
            {company}
          </div>
          <div className="bar-track">
            <div
              className="bar-fill"
              style={{ width: `${Math.max(6, (count / max) * 100)}%` }}
            />
          </div>
          <div className="bar-value">{count}</div>
        </div>
      ))}
    </div>
  );
}
