interface Props {
  search: string;
  onSearchChange: (v: string) => void;
  company: string;
  onCompanyChange: (v: string) => void;
  companies: string[];
}

export function Filters({
  search,
  onSearchChange,
  company,
  onCompanyChange,
  companies,
}: Props) {
  return (
    <div className="filters">
      <input
        type="text"
        placeholder="Search title, company, or description…"
        value={search}
        onChange={(e) => onSearchChange(e.target.value)}
        aria-label="Search jobs"
      />
      <select
        value={company}
        onChange={(e) => onCompanyChange(e.target.value)}
        aria-label="Filter by company"
      >
        <option value="">All companies</option>
        {companies.map((c) => (
          <option key={c} value={c}>
            {c}
          </option>
        ))}
      </select>
    </div>
  );
}
